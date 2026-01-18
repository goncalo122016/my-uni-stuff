// import { dataCache } from './cache.js';

// Importar dados - usando dynamic import para evitar overhead
let dbData = null;

async function loadDatabase() {
  if (dbData) return dbData;
  try {
    const response = await fetch('/data/db.json');
    dbData = await response.json();
    return dbData;
  } catch (e) {
    console.warn('Erro ao carregar db.json:', e);
    return {};
  }
}

const API_BASE_URL = 'http://localhost:3000';

/**
 * Map quadrimestre to month ranges (Portuguese)
 * Quadrimestre = 3 meses (4 quadrimestres por ano)
 * Q1 = Jan-Mar (months 1-3)
 * Q2 = Apr-Jun (months 4-6)
 * Q3 = Jul-Sep (months 7-9)
 * Q4 = Oct-Dec (months 10-12)
 */
function getMonthRangeForPeriod(period) {
  if (!period) return null;

  // Extract quadrimestre from format like "Q1 2024", "Q2 2024", etc.
  const match = period.match(/Q(\d)/);
  if (!match) return null;

  const q = parseInt(match[1]);
  switch (q) {
    case 1:
      return { start: 1, end: 3 }; // Jan-Mar (3 meses)
    case 2:
      return { start: 4, end: 6 }; // Apr-Jun (3 meses)
    case 3:
      return { start: 7, end: 9 }; // Jul-Sep (3 meses)
    case 4:
      return { start: 10, end: 12 }; // Oct-Dec (3 meses)
    default:
      return null;
  }
}

/**
 * Filter listings and reviews by period based on last_review date
 */
function filterDataByPeriod(data, period) {
  if (!period || !data) return data;

  const monthRange = getMonthRangeForPeriod(period);
  if (!monthRange) return data;

  console.log(
    `🔍 Filtrando dados para período ${period} (meses ${monthRange.start}-${monthRange.end})`
  );

  // Filter listings based on last_review date
  const filteredListings = data.listings.filter((listing) => {
    if (!listing.last_review) return false;

    const reviewDate = new Date(listing.last_review);
    const reviewMonth = reviewDate.getMonth() + 1; // getMonth returns 0-11, so add 1

    return reviewMonth >= monthRange.start && reviewMonth <= monthRange.end;
  });

  // Filter reviews by date range as well
  const filteredReviews = (data.reviews || []).filter((review) => {
    if (!review.date) return false;

    const reviewDate = new Date(review.date);
    const reviewMonth = reviewDate.getMonth() + 1;

    return reviewMonth >= monthRange.start && reviewMonth <= monthRange.end;
  });

  console.log(`   Filtrado: ${filteredListings.length}/${data.listings.length} listings`);

  return {
    ...data,
    listings: filteredListings,
    reviews: filteredReviews,
  };
}

/**
 * Fetch data for a specific city from the JSON Server
 * Usa cache para evitar múltiplas requisições
 */
export async function loadCityData(city, period = null, forceReload = true) {
  try {
    console.log(`🌐 Carregando ${city}...${period ? ` (período: ${period})` : ''}`);

    // Carregar dados do db.json
    const db = await loadDatabase();

    const cityKey = `${city}.listings`;
    const reviewsKey = `${city}.reviews`;
    const neighbourhoodsKey = `${city}.neighbourhoods`;

    const listings = db[cityKey] || [];
    const reviews = db[reviewsKey] || [];
    const neighbourhoods = db[neighbourhoodsKey] || [];

    let data = {
      city,
      listings,
      reviews,
      neighbourhoods,
    };

    // Apply period filtering if period is specified
    if (period) {
      data = filterDataByPeriod(data, period);
    }

    return data;
  } catch (error) {
    console.error(`Error loading data for ${city}:`, error);
    return { city, listings: [], reviews: [], neighbourhoods: [] };
  }
}

/**
 * Calculate key performance indicators from listings data
 */
export function calculateKPIs(data) {
  if (!data || !data.listings || data.listings.length === 0) {
    return {
      totalListings: 0,
      avgPrice: 0,
      avgReviews: 0,
      occupancyRate: 0,
      activeListings: 0,
    };
  }

  const listings = data.listings;
  const totalListings = listings.length;

  // Calculate average price
  const pricesSum = listings.reduce((sum, listing) => {
    const price = parseFloat(listing.price);
    return sum + (isNaN(price) ? 0 : price);
  }, 0);
  const avgPrice = pricesSum / listings.filter((l) => !isNaN(parseFloat(l.price))).length;

  // Calculate average reviews
  const totalReviews = listings.reduce((sum, listing) => sum + (listing.number_of_reviews || 0), 0);
  const avgReviews = totalReviews / totalListings;

  // Calculate occupancy rate (based on reviews per month and availability)
  const occupancyRate =
    listings.reduce((sum, listing) => {
      const rpm = listing.reviews_per_month || 0;
      return sum + Math.min(100, rpm * 3.33); // Assuming avg 3.33 reviews per month = high occupancy
    }, 0) / totalListings;

  // Count active listings (with availability)
  const activeListings = listings.filter((l) => (l.availability_365 || 0) < 300).length;

  return {
    totalListings,
    avgPrice: Math.round(avgPrice),
    avgReviews: Math.round(avgReviews * 10) / 10,
    occupancyRate: Math.round(occupancyRate),
    activeListings,
  };
}

/**
 * Group listings by room type
 */
export function groupByRoomType(data) {
  if (!data || !data.listings) return {};

  const grouped = {};
  data.listings.forEach((listing) => {
    const roomType = listing.room_type || 'Unknown';
    if (!grouped[roomType]) {
      grouped[roomType] = 0;
    }
    grouped[roomType]++;
  });

  return grouped;
}

/**
 * Group listings by price range
 */
export function groupByPriceRange(data) {
  if (!data || !data.listings) return {};

  const ranges = {
    '0-50': 0,
    '50-100': 0,
    '100-200': 0,
    '200-300': 0,
    '300+': 0,
  };

  data.listings.forEach((listing) => {
    const price = parseFloat(listing.price);
    if (isNaN(price)) return;

    if (price < 50) ranges['0-50']++;
    else if (price < 100) ranges['50-100']++;
    else if (price < 200) ranges['100-200']++;
    else if (price < 300) ranges['200-300']++;
    else ranges['300+']++;
  });

  return ranges;
}

/**
 * Get top listings by number of reviews
 */
export function getTopListings(data, limit = 10) {
  if (!data || !data.listings) return [];

  return data.listings
    .sort((a, b) => (b.number_of_reviews || 0) - (a.number_of_reviews || 0))
    .slice(0, limit)
    .map((listing) => {
      const availability = listing.availability_365 || 365;
      const occupancy = Math.round(((365 - availability) / 365) * 100);
      return {
        id: listing.id,
        name: listing.name || 'Sem nome',
        price: listing.price ? `€${listing.price}` : 'N/A',
        reviews: listing.number_of_reviews || 0,
        neighbourhood: listing.neighbourhood || 'Desconhecido',
        roomType: listing.room_type || 'Desconhecido',
        host: listing.host_name || 'Desconhecido',
        avgReviewsPerMonth: listing.reviews_per_month || 0,
        occupancy: Math.max(0, Math.min(100, occupancy)),
      };
    });
}

/**
 * Group listings by neighbourhood
 */
export function groupByNeighbourhood(data) {
  if (!data || !data.listings) return {};

  const grouped = {};
  data.listings.forEach((listing) => {
    const neighbourhood = listing.neighbourhood || 'Unknown';
    if (!grouped[neighbourhood]) {
      grouped[neighbourhood] = {
        count: 0,
        avgPrice: 0,
        totalPrice: 0,
        avgReviews: 0,
        totalReviews: 0,
      };
    }
    grouped[neighbourhood].count++;
    const price = parseFloat(listing.price);
    if (!isNaN(price)) {
      grouped[neighbourhood].totalPrice += price;
    }
    grouped[neighbourhood].totalReviews += listing.number_of_reviews || 0;
  });

  // Calculate averages
  Object.keys(grouped).forEach((neighbourhood) => {
    const data = grouped[neighbourhood];
    data.avgPrice = Math.round(data.totalPrice / data.count);
    data.avgReviews = Math.round(data.totalReviews / data.count);
  });

  return grouped;
}

/**
 * Count all listings for a specific city
 */
export async function countAllListings(city) {
  try {
    const data = await loadCityData(city);
    return data.listings.length;
  } catch (error) {
    console.error('Error counting listings:', error);
    return 0;
  }
}

/**
 * Export city data in multiple formats (CSV or JSON)
 */
export async function exportCityData(city, format = 'csv') {
  try {
    const data = await loadCityData(city);
    if (!data || !data.listings || data.listings.length === 0) {
      alert('Sem dados disponíveis para exportar');
      return;
    }

    switch (format.toLowerCase()) {
      case 'csv':
        exportAsCSV(data.listings, `${city}-listings`);
        break;
      case 'json':
        exportAsJSON(data, `${city}-data`);
        break;
      case 'xlsx':
        console.log('Exportando como XLSX...');
        const xlsxBlob = await exportAsXLSX(data.listings);
        downloadFile(xlsxBlob, `${city}-listings.xlsx`, "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        break;
      default:
        console.error('Formato não suportado:', format);
    }
  } catch (error) {
    console.error('Erro ao exportar dados:', error);
    alert('Erro ao exportar dados');
  }
}

/**
 * Export city data filtered by time period
 */
export async function exportCityDataByPeriod(city, format, period = 'Q1 2024') {
  try {
    const data = await loadCityData(city, period);
    if (!data || !data.listings || data.listings.length === 0) {
      alert('Sem dados disponíveis para exportar');
      return;
    }

    const periodLabel = period.replace(' ', '').replace(/\//g, '-');

    switch (format.toLowerCase()) {
      case 'csv':
        exportAsCSV(data.listings, `${city}-${periodLabel}`);
        break;
      case 'json':
        exportAsJSON(data, `${city}-${periodLabel}`);
        break;
      case 'xlsx':
        console.log('Exportando como XLSX...');
        const xlsxBlob = await exportAsXLSX(data.listings);
        downloadFile(xlsxBlob, `${city}-${periodLabel}.xlsx`, "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        break;
      default:
        console.error('Formato não suportado:', format);
    }
  } catch (error) {
    console.error('Erro ao exportar dados por período:', error);
    alert('Erro ao exportar dados');
  }
}

/**
 * Export data as CSV
 */
function exportAsCSV(listings, filename) {
  const headers = [
    'ID',
    'Name',
    'Price',
    'Room Type',
    'Reviews',
    'Neighbourhood',
    'Host',
    'Availability',
  ];
  const rows = listings.map((listing) => [
    listing.id || '',
    `"${(listing.name || '').replace(/"/g, '""')}"`,
    listing.price || '',
    listing.room_type || '',
    listing.number_of_reviews || 0,
    listing.neighbourhood || '',
    `"${(listing.host_name || '').replace(/"/g, '""')}"`,
    listing.availability_365 || 0,
  ]);

  const csv = [headers, ...rows].map((row) => row.join(',')).join('\n');
  downloadFile(csv, `${filename}.csv`, 'text/csv;charset=utf-8;');
}

/**
 * Export data as JSON
 */
function exportAsJSON(data, filename) {
  const json = JSON.stringify(data, null, 2);
  downloadFile(json, `${filename}.json`, 'application/json;charset=utf-8;');
}

/**
 * Export data as XLSX
 */

async function exportAsXLSX(data) {
  const XLSX = await import("xlsx");
  const worksheet = XLSX.utils.json_to_sheet(data);
  const workbook = XLSX.utils.book_new();
  XLSX.utils.book_append_sheet(workbook, worksheet, "Listings");

  const array = XLSX.write(workbook, { bookType: "xlsx", type: "array" });

  return new Blob([array], {
    type: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  });
}

/**
 * Export comprehensive price analysis as XLSX with multiple sheets
 */
export async function exportPriceAnalysis(city, period = null) {
  try {
    const data = await loadCityData(city, period);
    if (!data || !data.listings || data.listings.length === 0) {
      alert('Sem dados disponíveis para exportar');
      return;
    }

    const XLSX = await import('xlsx');
    const workbook = XLSX.utils.book_new();

    // Sheet 1: Summary Statistics
    const summaryData = generateSummarySheet(data);
    const summarySheet = XLSX.utils.json_to_sheet(summaryData);
    XLSX.utils.book_append_sheet(workbook, summarySheet, 'Summary');

    // Sheet 2: Price Distribution
    const priceDistData = generatePriceDistributionSheet(data);
    const priceDistSheet = XLSX.utils.json_to_sheet(priceDistData);
    XLSX.utils.book_append_sheet(workbook, priceDistSheet, 'Price Distribution');

    // Sheet 3: By Property Type
    const propertyTypeData = generatePropertyTypeSheet(data);
    const propertyTypeSheet = XLSX.utils.json_to_sheet(propertyTypeData);
    XLSX.utils.book_append_sheet(workbook, propertyTypeSheet, 'By Property Type');

    // Sheet 4: Top Neighborhoods
    const neighbourhoodData = generateNeighbourhoodSheet(data);
    const neighbourhoodSheet = XLSX.utils.json_to_sheet(neighbourhoodData);
    XLSX.utils.book_append_sheet(workbook, neighbourhoodSheet, 'Top Neighborhoods');

    // Sheet 5: Detailed Listings (top 100 by reviews)
    const detailedListings = data.listings
      .sort((a, b) => (b.number_of_reviews || 0) - (a.number_of_reviews || 0))
      .slice(0, 100)
      .map((listing) => ({
        ID: listing.id,
        Name: listing.name || 'Sem nome',
        Price: parseFloat(listing.price) || 0,
        'Room Type': listing.room_type || 'N/A',
        Neighbourhood: listing.neighbourhood || 'N/A',
        Reviews: listing.number_of_reviews || 0,
        'Reviews/Month': listing.reviews_per_month || 0,
        'Availability (days)': listing.availability_365 || 0,
        'Occupancy Rate %': Math.round(
          ((365 - (listing.availability_365 || 365)) / 365) * 100
        ),
        Host: listing.host_name || 'N/A',
        Latitude: listing.latitude || '',
        Longitude: listing.longitude || '',
      }));
    const detailedSheet = XLSX.utils.json_to_sheet(detailedListings);
    XLSX.utils.book_append_sheet(workbook, detailedSheet, 'Top 100 Listings');

    // Generate file
    const periodLabel = period ? `-${period.replace(' ', '').replace(/\//g, '-')}` : '';
    const filename = `${city}${periodLabel}-price-analysis.xlsx`;

    const array = XLSX.write(workbook, { bookType: 'xlsx', type: 'array' });
    const blob = new Blob([array], {
      type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
    });

    downloadFile(blob, filename, blob.type);
    console.log(`✅ Exportação concluída: ${filename}`);
  } catch (error) {
    console.error('Erro ao exportar análise de preço:', error);
    alert('Erro ao exportar análise de preço');
  }
}

/**
 * Generate summary statistics sheet
 */
function generateSummarySheet(data) {
  const listings = data.listings;
  const prices = listings.map((l) => parseFloat(l.price)).filter((p) => !isNaN(p) && p > 0);
  prices.sort((a, b) => a - b);

  const totalListings = listings.length;
  const avgPrice = prices.reduce((a, b) => a + b, 0) / prices.length;
  const medianPrice = prices[Math.floor(prices.length / 2)];
  const minPrice = Math.min(...prices);
  const maxPrice = Math.max(...prices);

  // Calculate percentiles
  const p25 = prices[Math.floor(prices.length * 0.25)];
  const p75 = prices[Math.floor(prices.length * 0.75)];
  const p90 = prices[Math.floor(prices.length * 0.90)];

  // Calculate occupancy
  const avgOccupancy =
    listings.reduce((sum, l) => {
      const avail = l.availability_365 || 365;
      return sum + ((365 - avail) / 365) * 100;
    }, 0) / totalListings;

  // Calculate revenue metrics
  const avgRevPerNight = avgPrice;
  const avgRevPerMonth = avgPrice * (avgOccupancy / 100) * 30;
  const avgRevPerYear = avgRevPerMonth * 12;

  return [
    { Metric: 'City', Value: data.city },
    { Metric: 'Total Listings', Value: totalListings },
    { Metric: '', Value: '' },
    { Metric: '=== PRICE STATISTICS ===', Value: '' },
    { Metric: 'Average Price (€/night)', Value: Math.round(avgPrice) },
    { Metric: 'Median Price (€/night)', Value: Math.round(medianPrice) },
    { Metric: 'Min Price (€/night)', Value: Math.round(minPrice) },
    { Metric: 'Max Price (€/night)', Value: Math.round(maxPrice) },
    { Metric: '', Value: '' },
    { Metric: '=== PERCENTILES ===', Value: '' },
    { Metric: '25th Percentile (€)', Value: Math.round(p25) },
    { Metric: '75th Percentile (€)', Value: Math.round(p75) },
    { Metric: '90th Percentile (€)', Value: Math.round(p90) },
    { Metric: '', Value: '' },
    { Metric: '=== OCCUPANCY & REVENUE ===', Value: '' },
    { Metric: 'Average Occupancy Rate (%)', Value: Math.round(avgOccupancy) },
    { Metric: 'Avg Revenue/Night (€)', Value: Math.round(avgRevPerNight) },
    { Metric: 'Avg Revenue/Month (€)', Value: Math.round(avgRevPerMonth) },
    { Metric: 'Avg Revenue/Year (€)', Value: Math.round(avgRevPerYear) },
  ];
}

/**
 * Generate price distribution sheet
 */
function generatePriceDistributionSheet(data) {
  const ranges = [
    { range: '0-50', min: 0, max: 50 },
    { range: '50-100', min: 50, max: 100 },
    { range: '100-150', min: 100, max: 150 },
    { range: '150-200', min: 150, max: 200 },
    { range: '200-300', min: 200, max: 300 },
    { range: '300-500', min: 300, max: 500 },
    { range: '500+', min: 500, max: Infinity },
  ];

  const distribution = ranges.map((r) => {
    const count = data.listings.filter((l) => {
      const price = parseFloat(l.price);
      return !isNaN(price) && price >= r.min && price < r.max;
    }).length;

    const percentage = ((count / data.listings.length) * 100).toFixed(1);

    return {
      'Price Range (€)': r.range,
      'Number of Listings': count,
      'Percentage (%)': parseFloat(percentage),
    };
  });

  return distribution;
}

/**
 * Generate property type analysis sheet
 */
function generatePropertyTypeSheet(data) {
  const types = {};

  data.listings.forEach((listing) => {
    const roomType = listing.room_type || 'Unknown';
    if (!types[roomType]) {
      types[roomType] = {
        count: 0,
        prices: [],
        occupancies: [],
        reviews: [],
      };
    }
    types[roomType].count++;

    const price = parseFloat(listing.price);
    if (!isNaN(price) && price > 0) {
      types[roomType].prices.push(price);
    }

    const avail = listing.availability_365 || 365;
    const occupancy = ((365 - avail) / 365) * 100;
    types[roomType].occupancies.push(occupancy);

    types[roomType].reviews.push(listing.number_of_reviews || 0);
  });

  return Object.entries(types).map(([type, stats]) => {
    const avgPrice =
      stats.prices.length > 0
        ? stats.prices.reduce((a, b) => a + b, 0) / stats.prices.length
        : 0;
    const avgOccupancy =
      stats.occupancies.reduce((a, b) => a + b, 0) / stats.occupancies.length;
    const avgReviews = stats.reviews.reduce((a, b) => a + b, 0) / stats.reviews.length;

    stats.prices.sort((a, b) => a - b);
    const medianPrice = stats.prices[Math.floor(stats.prices.length / 2)] || 0;

    return {
      'Property Type': type,
      'Count': stats.count,
      'Avg Price (€/night)': Math.round(avgPrice),
      'Median Price (€/night)': Math.round(medianPrice),
      'Avg Occupancy (%)': Math.round(avgOccupancy),
      'Avg Reviews': Math.round(avgReviews),
      'Est. Monthly Revenue (€)': Math.round(avgPrice * (avgOccupancy / 100) * 30),
    };
  });
}

/**
 * Generate neighborhood analysis sheet
 */
function generateNeighbourhoodSheet(data) {
  const neighbourhoods = {};

  data.listings.forEach((listing) => {
    const neighbourhood = listing.neighbourhood || 'Unknown';
    if (!neighbourhoods[neighbourhood]) {
      neighbourhoods[neighbourhood] = {
        count: 0,
        prices: [],
        reviews: [],
        occupancies: [],
      };
    }
    neighbourhoods[neighbourhood].count++;

    const price = parseFloat(listing.price);
    if (!isNaN(price) && price > 0) {
      neighbourhoods[neighbourhood].prices.push(price);
    }

    neighbourhoods[neighbourhood].reviews.push(listing.number_of_reviews || 0);

    const avail = listing.availability_365 || 365;
    const occupancy = ((365 - avail) / 365) * 100;
    neighbourhoods[neighbourhood].occupancies.push(occupancy);
  });

  const result = Object.entries(neighbourhoods)
    .map(([name, stats]) => {
      const avgPrice =
        stats.prices.length > 0
          ? stats.prices.reduce((a, b) => a + b, 0) / stats.prices.length
          : 0;
      const avgReviews = stats.reviews.reduce((a, b) => a + b, 0) / stats.reviews.length;
      const avgOccupancy =
        stats.occupancies.reduce((a, b) => a + b, 0) / stats.occupancies.length;

      return {
        Neighbourhood: name,
        'Listings Count': stats.count,
        'Avg Price (€/night)': Math.round(avgPrice),
        'Avg Reviews': Math.round(avgReviews),
        'Avg Occupancy (%)': Math.round(avgOccupancy),
        'Est. Monthly Revenue (€)': Math.round(avgPrice * (avgOccupancy / 100) * 30),
      };
    })
    .sort((a, b) => b['Avg Price (€/night)'] - a['Avg Price (€/night)'])
    .slice(0, 20); // Top 20 neighbourhoods

  return result;
}


function downloadFile(content, filename, mimeType) {
  const blob = new Blob([content], { type: mimeType });
  const link = document.createElement('a');
  link.href = URL.createObjectURL(blob);
  link.download = filename;
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
  URL.revokeObjectURL(link.href);
}

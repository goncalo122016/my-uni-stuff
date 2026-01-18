package server.event;

import java.util.Date;

public class Event {
    private Date date;
    private String productName;
    private double quantity;
    private double price;

    public Event(Date date, String productName, double quantity, double price) {
        this.date = date;
        this.productName = productName;
        this.quantity = quantity;
        this.price = price;
    }

    public Date getDate() {
        return date;
    }

    public String getProductName() {
        return productName;
    }

    public double getQuantity() {
        return quantity;
    }

    public double getPrice() {
        return price;
    }
}

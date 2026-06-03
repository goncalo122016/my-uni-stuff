#ifndef FRUSTUM_H
#define FRUSTUM_H

#include <Vec.hpp>
#include <Mat.hpp>
#include <algorithm>
#include <cstdint>

/**
 * @struct Plane
 * @brief Representa um plano matemático definido por um vetor normal e distância
 * 
 * Um plano é definido pela equação: normal · ponto - distance = 0
 * A distância com sinal de um ponto ao plano permite determinar em qual lado está
 */
struct Plane
{
    Vec3f normal = Vec3f{0.f, 1.f, 0.f};
    float distance = 0.f;

    Plane() = default;
    Plane(Vec3f point, Vec3f n) : normal(n.Normalized()), distance(n.Normalized().Dot(point)) {}

    float getSignedDistanceToPlane(const Vec3f &point) const
    {
        return normal.Dot(point) - distance;
    }
};

/**
 * @struct AABB
 * @brief Axis-Aligned Bounding Box - Caixa delimitadora alinhada com os eixos
 * 
 * Representa um volume rectangular em 3D com lados paralelos aos eixos X, Y, Z
 * Definida por dois pontos: canto mínimo (inferior-esquerdo-traseiro) 
 * e canto máximo (superior-direito-frontal)
 */
struct AABB
{
    Vec3f min;  // Canto inferior-esquerdo-traseiro
    Vec3f max;  // Canto superior-direito-frontal
    bool m_valid = false;  // Flag para rastrear validade do AABB

    /**
     * @brief Construtor padrão que cria um AABB inválido
     */
    AABB() = default;

    /**
     * @brief Construtor com valores específicos
     * @param min_val Ponto mínimo
     * @param max_val Ponto máximo
     */
    AABB(const Vec3f &min_val, const Vec3f &max_val) : min(min_val), max(max_val) {}

    /**
     * @brief Verifica se o AABB é válido (tem valores numéricos)
     * @return true se o AABB é válido, false caso contrário
     */
    bool IsValid() const
    {
        return m_valid;
    }

    /**
     * @brief Estende o AABB para incluir um novo ponto
     * @param point Ponto a incluir no AABB
     * 
     * Se o AABB é inválido, é inicializado com este ponto
     * Caso contrário, expande-se para incluir o ponto
     */
    void Extend(const Vec3f &point)
    {
        if (!m_valid)
        {
            min = point;
            max = point;
            m_valid = true;
        }
        else
        {
            min.x = std::min(min.x, point.x);
            min.y = std::min(min.y, point.y);
            min.z = std::min(min.z, point.z);

            max.x = std::max(max.x, point.x);
            max.y = std::max(max.y, point.y);
            max.z = std::max(max.z, point.z);
        }
    }

    /**
     * @brief Calcula o centro do AABB
     * @return Centro do AABB
     */
    Vec3f GetCenter() const
    {
        return {
            (min.x + max.x) * 0.5f,
            (min.y + max.y) * 0.5f,
            (min.z + max.z) * 0.5f
        };
    }

    /**
     * @brief Calcula as extensões (metade das dimensões) do AABB
     * @return Vetor com as extensões em x, y, z
     */
    Vec3f GetExtents() const
    {
        return {
            (max.x - min.x) * 0.5f,
            (max.y - min.y) * 0.5f,
            (max.z - min.z) * 0.5f
        };
    }

    /**
     * @brief Transforma o AABB aplicando uma matriz 4x4
     * @param matrix Matriz de transformação
     * @return Novo AABB transformado
     * 
     * O novo AABB é recalculado a partir dos 8 vértices transformados
     */
    AABB Transform(const Mat4f &matrix) const
    {
        if (!IsValid())
            return AABB();

        // Calcula os 8 vértices do AABB
        Vec3f vertices[8] = {
            {min.x, min.y, min.z},
            {max.x, min.y, min.z},
            {min.x, max.y, min.z},
            {max.x, max.y, min.z},
            {min.x, min.y, max.z},
            {max.x, min.y, max.z},
            {min.x, max.y, max.z},
            {max.x, max.y, max.z}
        };

        // Transforma todos os vértices
        AABB transformed;
        for (int i = 0; i < 8; ++i)
        {
            Vec4f v = vertices[i].ToVec4f();
            Vec4f transformed_v = matrix * v;
            transformed.Extend({transformed_v.x, transformed_v.y, transformed_v.z});
        }

        return transformed;
    }

    /**
     * @brief Testa se o AABB está à frente ou no plano (interseção com plano)
     * @param plane Plano para o qual testar
     * @return true se o AABB não está completamente atrás do plano
     * 
     * Algoritmo:
     * 1. Calcula o centro e extensões do AABB
     * 2. Projeta as extensões para a direção normal do plano (valor absoluto)
     * 3. Compara: raio >= distância(centro, plano)
     *    Se verdadeiro, o AABB interseça ou está à frente do plano
     */
    bool isOnOrForwardPlane(const Plane &plane) const
    {
        if (!IsValid())
            return true;

        // Calcula o centro e extensões do AABB
        const Vec3f center = GetCenter();
        const Vec3f extents = GetExtents();

        // Calcula o raio do AABB projectado na direcção da normal do plano
        // raio = |extents.x * |normal.x| + extents.y * |normal.y| + extents.z * |normal.z||
        const float radius = 
            extents.x * std::abs(plane.normal.x) +
            extents.y * std::abs(plane.normal.y) +
            extents.z * std::abs(plane.normal.z);

        // Distância do centro ao plano
        const float distance = plane.getSignedDistanceToPlane(center);

        // Se a distância é maior ou igual ao raio negativo, está à frente ou interseça
        return distance >= -radius;
    }
};

/**
 * @struct Frustum
 * @brief Representa o frustum de visualização (pirâmide de visão da câmara)
 * 
 * O frustum é definido por 6 planos:
 * - Near e Far: planos próximo e distante (profundidade)
 * - Left e Right: planos esquerdo e direito (largura)
 * - Top e Bottom: planos superior e inferior (altura)
 */
struct Frustum
{
    Plane topFace;      // Plano superior
    Plane bottomFace;   // Plano inferior
    Plane rightFace;    // Plano direito
    Plane leftFace;     // Plano esquerdo
    Plane farFace;      // Plano distante (far plane)
    Plane nearFace;     // Plano próximo (near plane)

    /**
     * @brief Testa se um AABB está dentro (ou interseça) o frustum
     * @param aabb Caixa delimitadora para testar
     * @return true se o AABB está completamente dentro ou interseça o frustum
     *
     * O AABB está dentro se passar em todos os 6 testes de plano
     */
    bool HasInside(const AABB &aabb) const
    {
        return aabb.isOnOrForwardPlane(leftFace) &&
               aabb.isOnOrForwardPlane(rightFace) &&
               aabb.isOnOrForwardPlane(topFace) &&
               aabb.isOnOrForwardPlane(bottomFace) &&
               aabb.isOnOrForwardPlane(nearFace) &&
               aabb.isOnOrForwardPlane(farFace);
    }

    // Plane index mapping for the bitmask (bits 0-5):
    //   0=left  1=right  2=top  3=bottom  4=near  5=far
    // inMask: which planes to test (cleared bit = ancestor was already fully inside)
    // outMask: refined mask for children (planes where AABB is fully inside get cleared)
    // Returns false if AABB is completely outside any active plane → cull the subtree.
    bool HasInsideWithMask(const AABB &aabb, uint8_t inMask, uint8_t &outMask) const
    {
        if (!aabb.IsValid()) { outMask = inMask; return true; }

        const Plane *planes[6] = {
            &leftFace, &rightFace, &topFace, &bottomFace, &nearFace, &farFace
        };
        const Vec3f center  = aabb.GetCenter();
        const Vec3f extents = aabb.GetExtents();
        outMask = inMask;

        for (int i = 0; i < 6; i++) {
            if (!(inMask & (uint8_t)(1u << i))) continue; // already fully inside from ancestor
            const float radius =
                extents.x * std::abs(planes[i]->normal.x) +
                extents.y * std::abs(planes[i]->normal.y) +
                extents.z * std::abs(planes[i]->normal.z);
            const float dist = planes[i]->getSignedDistanceToPlane(center);
            if (dist < -radius) return false;          // fully outside → cull
            if (dist >  radius) outMask &= ~(uint8_t)(1u << i); // fully inside → children skip this plane
        }
        return true;
    }
};

/**
 * @brief Cria um frustum a partir dos parâmetros da câmara
 * @param cameraPosition Posição da câmara no espaço global
 * @param cameraLookingAt Ponto para o qual a câmara olha
 * @param cameraUp Vetor "up" da câmara (normalmente (0, 1, 0))
 * @param fovDegrees Field of View em graus (ângulo vertical)
 * @param aspectRatio Razão de aspecto (largura/altura)
 * @param near Plano próximo (z próximo)
 * @param far Plano distante (z distante)
 * @return Frustum calculado
 * 
 * Algoritmo:
 * 1. Calcula dimensões da vista (altura e largura do plano far)
 * 2. Calcula os 3 vetores ortogonais da câmara (front, right, up)
 * 3. Define os 6 planos usando estes vetores
 */
Frustum CreateFrustumFromCamera(
    const Vec3f &cameraPosition,
    const Vec3f &cameraLookingAt,
    const Vec3f &cameraUp,
    float fovDegrees,
    float aspectRatio,
    float near,
    float far
);

#endif // FRUSTUM_H

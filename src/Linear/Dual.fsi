namespace Solver.Simplex


/// <summary>单纯形法对偶问题计算模块
/// </summary>
[<RequireQualifiedAccess>]
module internal Dual =

    /// <summary>单纯形法对偶问题类型
    /// </summary>
    type T

    /// <summary>创建单纯形法对偶问题
    /// </summary>
    /// <param name="da">对偶问题矩阵。</param>
    /// <param name="ai">或有的人工变量起始索引。</param>
    /// <returns>单纯形法对偶问题。</returns>
    val create: da: float[,] -> ai: int option -> T

    /// <summary>由原问题矩阵创建单纯形法对偶问题
    /// </summary>
    /// <param name="pa">原问题矩阵。</param>
    /// <param name="ai">或有的人工变量起始索引。</param>
    /// <returns>单纯形法对偶问题。</returns>
    val ofPrimal: pa: float[,] -> ai: int option -> T

    /// <summary>经典枢轴算法
    /// </summary>
    /// <param name="sim">待解的单纯形法对偶问题。</param>
    /// <param name="trans">问题矩阵行变换函数。</param>
    /// <returns>枢轴计算结果。</returns>
    val classic: sim: T -> trans: (int -> int -> float[,] -> int -> int -> unit) -> (int * int) list

    /// <summary>边际最优枢轴算法
    /// </summary>
    /// <param name="sim">待解的单纯形法对偶问题。</param>
    /// <param name="trans">问题矩阵行变换函数。</param>
    /// <returns>枢轴计算结果。</returns>
    val optimal: sim: T -> trans: (int -> int -> float[,] -> int -> int -> unit) -> (int * int) list

    /// <summary>求解单纯形法对偶问题
    /// </summary>
    /// <param name="pivot">枢轴计算函数。</param>
    /// <param name="trans">问题矩阵行变换函数。</param>
    /// <param name="sim">待解的单纯形法对偶问题。</param>
    /// <returns>单纯形法对偶问题最优解集合。</returns>
    val solve:
        pivot: (T -> (int -> int -> float[,] -> int -> int -> unit) -> (int * int) list) ->
        trans: (int -> int -> float[,] -> int -> int -> unit) ->
        sim: T ->
            (float[,] * int[]) list

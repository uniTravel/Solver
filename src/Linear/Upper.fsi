namespace Solver.Linear

open System.Collections.Generic


/// <summary>上界法计算模块
/// </summary>
[<RequireQualifiedAccess>]
module internal Upper =

    /// <summary>上界法问题类型
    /// </summary>
    type T

    /// <summary>创建上界法问题
    /// </summary>
    /// <param name="pa">原问题矩阵。</param>
    /// <param name="u">上界字典。</param>
    /// <param name="ai">或有的人工变量起始索引。</param>
    /// <returns>上界法问题。</returns>
    val create: pa: float[,] -> u: IDictionary<int, float> -> ai: int option -> T

    /// <summary>经典枢轴算法
    /// </summary>
    /// <param name="sim">待解的上界法问题。</param>
    /// <param name="trans">问题矩阵行变换函数。</param>
    /// <returns>枢轴计算结果。</returns>
    val classic: sim: T -> trans: (int -> int -> float[,] -> int -> int -> unit) -> (int * int) list

    /// <summary>边际最优枢轴算法
    /// </summary>
    /// <param name="sim">待解的上界法问题。</param>
    /// <param name="trans">问题矩阵行变换函数。</param>
    /// <returns>枢轴计算结果。</returns>
    val optimal: sim: T -> trans: (int -> int -> float[,] -> int -> int -> unit) -> (int * int) list

    /// <summary>求解上界法问题
    /// </summary>
    /// <param name="pivot">枢轴计算函数。</param>
    /// <param name="trans">问题矩阵行变换函数。</param>
    /// <param name="sim">待解的上界法问题。</param>
    /// <returns>上界法问题最优解集合。</returns>
    val solve:
        pivot: (T -> (int -> int -> float[,] -> int -> int -> unit) -> (int * int) list) ->
        trans: (int -> int -> float[,] -> int -> int -> unit) ->
        sim: T ->
            (float[,] * int[] * IDictionary<int, ref<bool>>) list

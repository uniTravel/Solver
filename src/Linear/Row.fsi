namespace Solver.Linear


/// <summary>单纯形法行变换模块
/// </summary>
/// <remarks>规划问题矩阵元素为 float 类型。</remarks>
[<RequireQualifiedAccess>]
module internal Row =

    /// <summary>同步变换
    /// </summary>
    /// <param name="n">矩阵最大索引。</param>
    /// <param name="m">基阵的秩。</param>
    /// <param name="a">原问题矩阵。</param>
    /// <param name="pi">枢轴行索引。</param>
    /// <param name="pj">枢轴列索引。</param>
    val trans: n: int -> m: int -> a: float[,] -> pi: int -> pj: int -> unit

    /// <summary>异步变换
    /// </summary>
    /// <param name="n">矩阵最大索引。</param>
    /// <param name="m">基阵的秩。</param>
    /// <param name="a">原问题矩阵。</param>
    /// <param name="pi">枢轴行索引。</param>
    /// <param name="pj">枢轴列索引。</param>
    val atrans: n: int -> m: int -> a: float[,] -> pi: int -> pj: int -> unit

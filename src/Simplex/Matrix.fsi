namespace Solver.Simplex


/// <summary>用于单纯形法的矩阵计算模块
/// </summary>
[<RequireQualifiedAccess>]
module internal Matrix =

    /// <summary>行向量×列向量
    /// </summary>
    /// <remarks>主要用于中间计算。</remarks>
    /// <param name="left">左乘数行向量。</param>
    /// <param name="right">右乘数列向量。</param>
    val vv :
        left    : float [] ->
        right   : float []
                -> float

    /// <summary>行向量×矩阵
    /// </summary>
    /// <remarks>主要用于计算目标函数的相关系数与值。</remarks>
    /// <param name="left">左乘数行向量。</param>
    /// <param name="right">右乘数矩阵。</param>
    val vm :
        left    : float [] ->
        right   : float [,]
                -> float []

    /// <summary>矩阵×列向量
    /// </summary>
    /// <remarks>主要用于计算函数约束方程的右端项。</remarks>
    /// <param name="left">左乘数矩阵。</param>
    /// <param name="right">右乘数列向量。</param>
    val mv :
        left    : float [,] ->
        right   : float []
                -> float []

    /// <summary>矩阵×矩阵
    /// </summary>
    /// <remarks>主要用于计算函数约束方程的相关系数。</remarks>
    /// <param name="left">左乘数矩阵。</param>
    /// <param name="right">右乘数矩阵。</param>
    val mm :
        left    : float [,] ->
        right   : float [,]
                -> float [,]

    /// <summary>矩阵求逆
    /// </summary>
    /// <remarks>主要用于基矩阵求逆。</remarks>
    /// <param name="matrix">原矩阵。</param>
    val inv :
        matrix  : float [,]
                -> float [,]
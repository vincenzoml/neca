using System;
using System.Drawing;
using System.Windows.Forms;
using System.Drawing.Drawing2D;

public class PictureBoxWithInterpolationMode : PictureBox
{
    public InterpolationMode InterpolationMode { get; set; }

    protected override void OnPaint(PaintEventArgs paintEventArgs)
    {
        paintEventArgs.Graphics.InterpolationMode = InterpolationMode;
        base.OnPaint(paintEventArgs);
    }
}

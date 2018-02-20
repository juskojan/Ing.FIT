using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using BIO.Framework.Extensions.Emgu.InputData;
using BIO.Framework.Extensions.Emgu.FeatureVector;

using Emgu.CV;
using Emgu.CV.Structure;
using BIO.Framework.Core.FeatureVector;
using System.Drawing;

namespace BIO.Project.FingerprintRecognition
{
    class FingerprintFeatureVectorExtractor : IFeatureVectorExtractor<EmguGrayImageInputData, FingerprintFeatureVector>
    {

        public int getHorizontalRidgeCount(int axis, Image<Gray, Byte> skeleton)
        {
            int previous_col = 0;
            int ridge_count = 0;
            List<int> pixels = new List<int>();

            for (int col = 0; col < skeleton.Width; col++)
            {
                int current_col = 0;

                for (int row = 0; row < skeleton.Height; row++)
                {
                    for (int position = (skeleton.Height / 4 * (axis - 2)) - 2; position < (skeleton.Height / 4 * (axis - 2)) + 3; position++)
                    {
                        if (row == position && skeleton.Data[row, col, 0] > 0)
                        {
                            current_col = 255;
                            skeleton.Data[row, col, 0] = 100;
                        }

                        if (row == position)
                        {
                            //skeleton.Data[row, col, 0] = 150;
                        }
                    }
                }

                pixels.Add(current_col);

                previous_col = current_col;
            }

            skeleton.Save(@"C:\Users\Jusko\Desktop\2MIT\BIO\odtlacky\skeleton.png");

            for (int i = 1; i < pixels.Count - 2; i++)
            {
                if (pixels[i] == 0 && pixels[i - 1] == 255 && pixels[i + 1] == 0 && pixels[i + 2] == 255)
                {
                    pixels[i] = 255;
                    pixels[i + 1] = 255;
                }

                if (pixels[i] == 0 && pixels[i - 1] == 255 && pixels[i + 1] == 255)
                {
                    pixels[i] = 255;

                }
            }

            for (int i = 0; i < pixels.Count - 1; i++)
            {
                if (pixels[i] == 255 && pixels[i + 1] == 0)
                {
                    ridge_count++;
                }
            }

            return ridge_count;
        }


        public int getVerticalRidgeCount(int axis, Image<Gray, Byte> skeleton)
        {
            // RIDGE COUNT
            int previous_row = 0;
            int ridge_count = 0;
            List<int> pixels = new List<int>();

            for (int v = 0; v < skeleton.Height; v++)
            {
                int current_row = 0;

                for (int u = 0; u < skeleton.Width; u++)
                {
                    for (int position = (skeleton.Width / 3 * axis) - 2; position < (skeleton.Width / 3 * axis) + 3; position++)
                    {

                        if (u == position && skeleton.Data[v, u, 0] > 0)
                        {
                            current_row = 255;
                            //skeleton.Data[v, u, 0] = 150;

                        }

                        if (u == position)
                        {

                        }
                    }
                }
                pixels.Add(current_row);

                previous_row = current_row;
            }

            //skeleton.Save(@"C:\Users\Jusko\Desktop\2MIT\BIO\odtlacky\skeleton.png");

            for (int i = 1; i < pixels.Count - 1; i++)
            {
                if (pixels[i] == 0 && pixels[i - 1] == 255 && pixels[i + 1] == 255)
                {
                    pixels[i] = 255;
                }
            }

            for (int i = 0; i < pixels.Count - 1; i++)
            {
                if (pixels[i] == 255 && pixels[i + 1] == 0)
                {
                    ridge_count++;
                }
            }

            return ridge_count;
        }

        public int getRidgeCount(int axis, Image<Gray, Byte> skeleton)
        {
            int ridge_count = 0;

            if (axis <= 2)
            {
                ridge_count = getVerticalRidgeCount(axis, skeleton);
            }
            else
            {
                ridge_count = getHorizontalRidgeCount(axis, skeleton);
            }

            return ridge_count;
        }

        public FingerprintFeatureVector extractFeatureVector(EmguGrayImageInputData input)
        {
            var input_img = input.Image.Clone();

            Image<Gray, byte> grayImg = input_img.Convert<Gray, byte>();
            Image<Gray, byte> binImg = new Image<Gray, byte>(grayImg.Size);

            // OTSU threshold
            CvInvoke.cvThreshold(grayImg, binImg, 100, 255, Emgu.CV.CvEnum.THRESH.CV_THRESH_OTSU | Emgu.CV.CvEnum.THRESH.CV_THRESH_BINARY);

            // INVERT
            binImg = binImg.Not();

            // SKELETONIZATION
            Image<Gray, byte> skel = new Image<Gray, byte>(binImg.Size);
            for (int y = 0; y < skel.Height; y++)
                for (int x = 0; x < skel.Width; x++)
                    skel.Data[y, x, 0] = 0;

            Image<Gray, byte> img = skel.Copy();
            for (int y = 0; y < skel.Height; y++)
                for (int x = 0; x < skel.Width; x++)
                    img.Data[y, x, 0] = binImg.Data[y, x, 0];

            StructuringElementEx element;
            element = new StructuringElementEx(3, 3, 1, 1, Emgu.CV.CvEnum.CV_ELEMENT_SHAPE.CV_SHAPE_CROSS);
            Image<Gray, byte> temp;

            bool done = false;
            do
            {
                temp = img.MorphologyEx(element, Emgu.CV.CvEnum.CV_MORPH_OP.CV_MOP_OPEN, 1);
                temp = temp.Not();
                temp = temp.And(img);
                skel = skel.Or(temp);
                img = img.Erode(1);
                double[] min, max;
                Point[] pmin, pmax;
                img.MinMax(out min, out max, out pmin, out pmax);
                done = (max[0] == 0);
            } while (!done);
                                                       
            // RIDGE COUNT
            FingerprintFeatureVector ridgeCounts = new FingerprintFeatureVector();

            ridgeCounts.Data = new int[6];

            for (int i = 1; i <= 5; i++)
            {
                int ridge_count = this.getRidgeCount(i, skel);
                
                ridgeCounts.Data[i] = ridge_count; 
                             
            }
            
            return ridgeCounts;
        }
    }
}

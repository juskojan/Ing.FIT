using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using BIO.Framework.Core.Comparator;
using BIO.Framework.Extensions.Emgu.FeatureVector;
using BIO.Framework.Extensions.Standard.Template;
using Emgu.CV.Structure;
using Emgu.CV;
using Emgu.CV.UI;
using BIO.Framework.Core;
using System.Collections;

namespace BIO.Project.FingerprintRecognition
{
    class FingerprintFeatureVectorComparator : IFeatureVectorComparator<FingerprintFeatureVector, FingerprintFeatureVector>
    {
        #region IFeatureVectorComparator<FingerprintFeatureVector,FingerprintFeatureVector> Members

        public MatchingScore computeMatchingScore(FingerprintFeatureVector extracted, FingerprintFeatureVector templated)
        {
            int size = extracted.Data.Length;

            double score = 0.0;

            for (int i = 0; i < size; i++)
            {
                score += Math.Abs(extracted.Data[i] - templated.Data[i]);

            }
            return new MatchingScore(score);            
        }

        #endregion
    }
}


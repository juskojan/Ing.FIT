using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using BIO.Framework.Core.FeatureVector;

namespace BIO.Project.FingerprintRecognition
{
    [Serializable]
    class FingerprintFeatureVector : IFeatureVector
    {
        public int[] Data
        {
            get;
            set;
        }
    }
}

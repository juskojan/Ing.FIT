using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using BIO.Framework.Extensions.Emgu.InputData;
using BIO.Framework.Extensions.Emgu.FeatureVector;
using BIO.Framework.Extensions.Standard.Template;
using BIO.Framework.Core.Comparator;
using BIO.Framework.Core.FeatureVector;

namespace BIO.Project.FingerprintRecognition
{
    abstract class FingerprintProcessingBlocks : BIO.Framework.Extensions.Standard.Block.InputDataProcessingBlockSettings<
        EmguGrayImageInputData,
        FingerprintFeatureVector,
        Template<FingerprintFeatureVector>,
        FingerprintFeatureVector>
    {
        public FingerprintProcessingBlocks(string name) : base(name)
        {

        }

        protected abstract IFeatureVectorExtractor<EmguGrayImageInputData, FingerprintFeatureVector> createFeatureVectorExtractor();

        //Extractor
        protected override IFeatureVectorExtractor<EmguGrayImageInputData, FingerprintFeatureVector> createTemplatedFeatureVectorExtractor()
        {
            return this.createFeatureVectorExtractor();
        }

        //Extractor
        protected override IFeatureVectorExtractor<EmguGrayImageInputData, FingerprintFeatureVector> createEvaluationFeatureVectorExtractor()
        {
            return this.createFeatureVectorExtractor();
        }

        //Comparator
        protected override Framework.Core.Comparator.IComparator<FingerprintFeatureVector, Template<FingerprintFeatureVector>, FingerprintFeatureVector> createComparator()
        {
            return new BIO.Framework.Extensions.Standard.Comparator.Comparator<FingerprintFeatureVector, Template<FingerprintFeatureVector>, FingerprintFeatureVector>(
                this.createFeatureVectorComparator(),
                this.createScoreSelector()
            );
        }

        //Comparator
        private IFeatureVectorComparator<FingerprintFeatureVector, FingerprintFeatureVector> createFeatureVectorComparator()
        {
            return new FingerprintFeatureVectorComparator();
        }

        //Selector
        private Framework.Extensions.Standard.Comparator.IScoreSelector createScoreSelector()
        {
            return new BIO.Framework.Extensions.Standard.Comparator.MinScoreSelector();
        }

    }

    class FingerprintProcessingBlock1 : FingerprintProcessingBlocks
    {

        public FingerprintProcessingBlock1()
            : base("Fingerprint Recognition - 1")
        {
        }

        protected override IFeatureVectorExtractor<EmguGrayImageInputData, FingerprintFeatureVector> createFeatureVectorExtractor()
        {
            return new FingerprintFeatureVectorExtractor();
        }
    }
}

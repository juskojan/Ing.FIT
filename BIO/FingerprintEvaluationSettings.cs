using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using BIO.Framework.Extensions.Standard.Database.InputDatabase;
using BIO.Framework.Extensions.Emgu.InputData;
using BIO.Framework.Extensions.Standard.Evaluation.Block;

namespace BIO.Project.FingerprintRecognition
{
    class FingerprintEvaluationSettings : BlockEvaluationSettings<StandardRecord<StandardRecordData>, EmguGrayImageInputData>
    {
        public FingerprintEvaluationSettings()
        {
            var value = new FingerprintProcessingBlock1();
            this.addBlockToEvaluation(value.createBlock());
        }

        protected override Framework.Core.InputData.IInputDataCreator<StandardRecord<StandardRecordData>, EmguGrayImageInputData> createInputDataCreator()
        {
            return new FingerprintInputDataCreator();
        }
    }
}

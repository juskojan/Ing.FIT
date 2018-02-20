using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using BIO.Framework.Extensions.Standard.Database.InputDatabase;
using BIO.Framework.Extensions.Emgu.InputData;

namespace BIO.Project.FingerprintRecognition
{
    class ProjectSettings :
        ProjectSettings<StandardRecord<StandardRecordData>, EmguGrayImageInputData>,
        IStandardProjectSettings<StandardRecord<StandardRecordData>>
    {
        public int TemplateSamples
        {
            get
            {
                return 1;
            }
        }

        public override Framework.Core.Database.IDatabaseCreator<StandardRecord<StandardRecordData>> getDatabaseCreator()
        {
            return new FingerprintDatabaseCreator(@"C:\Users\Jusko\Desktop\2MIT\BIO\odtlacky\PNG");        
        }

        protected override Framework.Core.Evaluation.Block.IBlockEvaluatorSettings<StandardRecord<StandardRecordData>, EmguGrayImageInputData> getEvaluatorSettings()
        {
            return new FingerprintEvaluationSettings();
        }
    }
}

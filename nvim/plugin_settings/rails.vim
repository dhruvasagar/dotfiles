let g:rails_gem_projections = {
      \ "carrierwave": {
      \   "app/uploaders/*_uploader.rb": {
      \     "command": "uploader",
      \     "template": "class %SUploader < CarrierWave::Uploader::Base\nend",
      \     "test": [
      \       "spec/uploaders/%s_uploader_spec.rb"
      \     ]
      \ }},
      \ "resque": {
      \   "app/jobs/*_job.rb": {
      \     "command": "job",
      \     "template": "class %SJob\n@queue = ''\nend",
      \     "test": [
      \       "spec/jobs/%s_job_spec.rb"
      \     ]
      \ }}
      \ }

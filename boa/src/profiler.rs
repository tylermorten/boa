#![allow(missing_copy_implementations, missing_debug_implementations)]

use measureme::{EventId, Profiler, TimingGuard};
use std::{path::Path, thread::current};

/// MmapSerializatioSink is faster on macOS and Linux
/// but FileSerializationSink is faster on Windows
#[cfg(not(windows))]
type SerializationSink = measureme::MmapSerializationSink;
#[cfg(windows)]
type SerializationSink = measureme::FileSerializationSink;

pub struct MyProfiler {
    profiler: Profiler<SerializationSink>,
}

impl MyProfiler {
    pub fn start_event(&self, label: &str) -> TimingGuard<'_, SerializationSink> {
        let kind = self.profiler.alloc_string("Generic");
        let id = EventId::from_label(self.profiler.alloc_string(label));
        let thread_id = current().id().as_u64() as u32;
        self.profiler
            .start_recording_interval_event(kind, id, thread_id)
    }

    pub fn new() -> MyProfiler {
        let profiler = Profiler::new(Path::new("./my_trace")).unwrap();
        MyProfiler { profiler }
    }
}

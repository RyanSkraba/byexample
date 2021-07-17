package com.skraba.byexample.lanterna.progress;

import java.io.IOException;

/** Used to monitor progress. */
public interface ProgressMonitor {

  void tick(int tickCount, int tickTotal) throws IOException;
}

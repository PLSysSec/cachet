// vim: set tw=99 ts=2 sts=2 sw=2 et:

'use strict';

import http from 'http';
import path from 'path';
import process from 'process';
import url from 'url';

import am from 'am';
import enableDestroy from 'server-destroy';
import Koa from 'koa';
import serve from 'koa-static';
import { Builder, By, until } from 'selenium-webdriver';
import firefox from 'selenium-webdriver/firefox.js';
import sleep from 'await-sleep';

const firefoxRootDirPath = '/home/spinda/workbench/research/mozilla-unified/objdirs/cachet';
const geckodriverExePath = path.join(
  firefoxRootDirPath,
  'testing',
  'geckodriver',
  'x86_64-unknown-linux-gnu',
  'release',
  'geckodriver'
);
const firefoxExePath = path.join(firefoxRootDirPath, 'dist', 'bin', 'firefox');

const evalDirPath = url.fileURLToPath(new URL('.', import.meta.url));;
const dataDirPath = path.join(evalDirPath, 'data');
const cacheIrSpewDirPath = path.join(dataDirPath, 'spew');

am(async () => {
  const useHeadlessFirefox = !process.argv.includes('--no-headless')

  console.log('Starting HTTP server...');

  const app = new Koa();
  app.use(serve('vendor/speedometer'));

  const server = http.createServer(app.callback());
  enableDestroy(server);
  const serverListeningPromise = new Promise((resolve, reject) => {
    server.once('listening', resolve);
    server.once('error', reject);
  });

  server.listen(0, '127.0.0.1');
  try {
    await serverListeningPromise;
    const serverAddr = server.address();

    console.log(`Starting Firefox${useHeadlessFirefox ? ' (headless)' : ''}...`);

    // Disable Firefox's content sandbox so content processes can write stub
    // files to disk.
    process.env.MOZ_DISABLE_CONTENT_SANDBOX = 1;
    process.env.JIT_OPTION_cacheIrSpewDirPath = cacheIrSpewDirPath;

    const firefoxOptions = new firefox.Options().setBinary(firefoxExePath);
    if (useHeadlessFirefox) {
      firefoxOptions.headless();
    }

    const driver = await new Builder()
      .forBrowser('firefox')
      .setFirefoxService(new firefox.ServiceBuilder(geckodriverExePath))
      .setFirefoxOptions(firefoxOptions)
      .build();

    try {
      console.log('Loading Speedometer...');

      await driver.get(`http://${serverAddr.address}:${serverAddr.port}`);

      const startButtonElem = await driver.findElement(By.css('#home .buttons button'));
      const progressTextElem = await driver.findElement(By.id('info'));
      const resultsScreenElem = await driver.findElement(By.id('summarized-results'));

      console.log('Running benchmark...');

      await startButtonElem.click();

      let isDone = false;
      const isDonePromise = (async () => {
        await driver.wait(until.elementIsVisible(resultsScreenElem));
        isDone = true;
      })();
      await Promise.all([
        isDonePromise,
        // Print progress to the console as the benchmark runs.
        (async () => {
          process.stdout.write(await progressTextElem.getText());
          while (!isDone) {
            const progressText = await progressTextElem.getText();
            process.stdout.clearLine(0);
            process.stdout.cursorTo(0);
            process.stdout.write(progressText);
            await Promise.race([isDonePromise, sleep(250)]);
          }
        })(),
      ]);

      process.stdout.clearLine(0);
      process.stdout.cursorTo(0);
      console.log('Done!');
    } finally {
      await driver.quit();
    }
  } finally {
    server.destroy();
  }
});

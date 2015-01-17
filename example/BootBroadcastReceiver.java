package com.dataember.android.service;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

/**
 * User: Stephen O'Brien
 * Date: 24/07/13
 * Time: 21:10
 *
 * Receives a boot message at boot and starts the login service.
 */
public class BootBroadcastReceiver extends BroadcastReceiver {

    @Override
    public void onReceive(Context context, Intent intent) {

        if(intent.getAction().equalsIgnoreCase(Intent.ACTION_BOOT_COMPLETED)) {
            Intent loginServiceIntent = new Intent(context, ControllerService.class);
            context.startService(loginServiceIntent);
        }

    }
}

const { EC2Client, StartInstancesCommand, StopInstancesCommand } = require("@aws-sdk/client-ec2");


exports.handler = async function StartStopInstanceHandler(evt) {
	console.debug("Initializing the EC2 client");
	const client = new EC2Client();
	console.debug("Looking up request", evt);
	const request = lookupRequest(evt);
	console.debug("Sending request", JSON.stringify(request, null, 2));
	const result = await client.send(request);
	console.debug("Sent request successfully", result);
	return result;
}

function lookupRequest(evt) {
	const { instanceId, command } = evt;
	if(!instanceId) {
		throw new Error(`No instance id in the event: ${JSON.stringify(evt)}`);
	}
	if(!command) {
		throw new Error(`No command in the event: ${JSON.stringify(evt)}`);
	}
	if(command === "start") {
		return new StartInstancesCommand({
			InstanceIds: [ instanceId ]
		});
	} else if (command === "stop") {
		return new StopInstancesCommand({
			InstanceIds: [ instanceId ]
		});
	} else {
		throw new Error(`Unrecognized command (expected: 'start' or 'stop'): ${command}`);
	}
}

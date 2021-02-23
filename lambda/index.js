const { EC2Client, StartInstancesCommand, StopInstancesCommand } = require("@aws-sdk/client-ec2");

const client = new EC2Client();

exports.handler = async function StartStopInstanceHandler(evt, ctx) {
	const request = lookupRequest(evt);
	await client.send(request);
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
		return new StartInstanceCommand({
			InstanceIds: [ instanceId ]
		});
	} else if (command === "stop") {
		return new StopInstanceCommand({
			InstanceIds: [ instanceId ]
		});
	} else {
		throw new Error(`Unrecognized command (expected: 'start' or 'stop'): ${command}`);
	}
}

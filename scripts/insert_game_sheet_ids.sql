create or replace function insert_game_sheet_ids(uuid, uuid[])
  returns void as $$
declare
  sheetId uuid;
begin
  foreach sheetId in array $2
  loop
    insert into game_sheet (game_id, sheet_id)
      values ($1, sheetId);
    raise notice 'inserted %', sheetId;
  end loop;
end;
$$ language 'plpgsql';

select * from insert_game_sheet_ids('c58ca50d-96e7-4aa9-9316-3a34becb7551'::uuid, array[
    '170cb7cf-468c-4164-8749-42a2459e530b'::uuid,
    'b74a39a8-b123-4a2e-8eae-6cc7c6eb5830',
    '3d444f97-7cea-4a9c-91b3-feed9c7b4dbe',
    'ee2cf772-2ebf-4da6-8a58-982f8b1c7444',
    '29f3427a-76c9-4125-837a-1ae17a26f74c',
    'be6d8a4d-cd09-4fd1-b920-69e61991261d',
    '5cb8eb95-2680-4584-9e72-c684de8ffae1',
    'a174c320-3a70-4b5a-9e94-49360dc3254c',
    '807d855e-8c65-4a55-9562-bb9feac760c7',
    '922aeff2-ee09-4313-b2e8-b59e22eb6c5d',
    'f67d89d5-d8fa-47a2-9894-7abfe4199c5f',
    '2c5c1970-5d75-4c4f-8a8d-428cbd7d60dc',
    'bc4910c7-00e0-467d-b4f7-7c8d12ae6ad4',
    '30bbd715-4dea-4bad-8fb4-0f5c360ddeb4',
    '7cecb800-7e97-433e-bc55-232c7d9b0b3a',
    '05d0b4a9-fb3b-4f04-9ccf-f5744c2a5f2d',
    'cb76a509-d7dd-45db-9655-f56511662e75',
    '815316cd-8fb9-4e9a-b49a-ebf7d956fd32',
    '8e684c2e-1807-4fc7-9247-e2472493336b',
    '4155507b-41e6-4ef1-9ba0-4aa6d7238cb4',
    'cbea516e-878a-49a8-8285-6d4ba5d7b0e9',
    'f09828c8-868a-42c4-88b7-49fa14cedcd3',
    '4f4a411e-9232-4989-b3e5-8846d1a00e0f',
    '285e7593-3a17-4dd5-a347-73e4fd3e68af',
    '2539b972-88a2-49be-91a5-65ebf3c1e007',
    'c8e26ca8-f442-43cc-b232-6ea3d87adf10',
    '423e8ca4-0fee-47e1-b216-c903c0d69ec8',
    '8fa2f1c9-91d9-4b23-a2d6-059603251c99',
    '1175ce4b-64f0-429c-9ff2-55c48b20f20f',
    '1d5f7c68-e294-4d8e-a1f0-3334e9af4127',
    '4c57bdf0-0cd9-4dff-8794-6275068b6fc5',
    '29866303-13bb-415b-8875-f7ece29fd0e7',
    'fe29aa4a-2010-4be6-9c07-e25498d50dbf',
    'fef1a3a5-04d1-4a33-8c64-a37ad9ba06d0',
    '3c4ff49c-824d-4f78-984a-174794c80de4',
    '425298e1-c430-4af0-80e4-4716fa353e3e',
    '943ef185-6304-4e70-b04e-6c04318406c7',
    'ab17a166-93d9-4ae7-8129-d1bf98032b3e',
    '43569204-ed55-4039-8ebc-3ddc5ca231b0',
    '21cabbe9-7d92-4372-999f-4663a023fa9a',
    'bbf4946a-fdcc-4d96-a056-a173d92ec75a',
    '6306bac6-da08-4632-b544-5bf95da619d8',
    'f47306f1-3202-4707-82d9-9fa227521c5d',
    '8d4ba18a-2e38-4cb8-aa28-9f8ff6451c54',
    '15c55530-0cc6-4478-b08c-982ad53b8218',
    'd82eb70b-bc21-482c-a628-8f2818301735',
    'a0c58d6d-4502-4ac6-93c3-9496bc6eab48',
    'e1811da3-b167-42ba-9890-d8c05ce49665',
    'f8a78f3b-45d6-4762-aff4-5eb070c0d531',
    '8ee56879-1b5b-42c2-9fed-90c8f9bc9f77',
    '3df86fcd-0621-4ae8-ab16-e1d00ce0cdcc',
    '9900cbdf-020c-4196-8d6a-084491d25cd9',
    'b9586ad1-87b7-4eea-9806-ac5b19223fec',
    '7904c8f9-c7e7-4f70-b9ac-67ee3b06aa31',
    '018f81a7-377b-420d-a82f-ccc20a57b29e',
    '549bc35b-8c88-468e-b4ee-e44adb33c8ad',
    '01c853f3-5119-42cc-8500-8d04e87d7ed3',
    '096e0214-8c1d-48a4-bb54-ada51e5c2287',
    '362dbeef-33bc-40b2-8717-850c7179a51b',
    'a3a65326-ee47-4d3e-b598-4c208092f2e6'
  ]);

-- Seed: 11199255121161820059,10557070023141912087

entity vnbzwwz is
  port (dywtzpgma : inout boolean_vector(4 to 0); hwqgkzza : out integer);
end vnbzwwz;

architecture sunusiqck of vnbzwwz is
  
begin
  -- Single-driven assignments
  hwqgkzza <= 2#1_1_1#;
end sunusiqck;

entity cqhcjaitar is
  port (hjs : in real; ojw : out boolean; ittora : buffer integer);
end cqhcjaitar;

architecture udnhf of cqhcjaitar is
  signal zju : integer;
  signal blh : boolean_vector(4 to 0);
  signal rsummexk : boolean_vector(4 to 0);
  signal cct : integer;
  signal jenhxgcz : boolean_vector(4 to 0);
begin
  bn : entity work.vnbzwwz
    port map (dywtzpgma => jenhxgcz, hwqgkzza => cct);
  ee : entity work.vnbzwwz
    port map (dywtzpgma => rsummexk, hwqgkzza => ittora);
  qmczkeand : entity work.vnbzwwz
    port map (dywtzpgma => blh, hwqgkzza => zju);
  
  -- Single-driven assignments
  ojw <= FALSE;
end udnhf;

entity qjjc is
  port (llzivjtb : inout time; gfczb : out integer; mylo : in real; m : inout bit_vector(3 to 0));
end qjjc;

architecture unshnigan of qjjc is
  signal yliiotz : boolean_vector(4 to 0);
  signal icwix : integer;
  signal smoqfs : boolean_vector(4 to 0);
  signal pbtflfoo : integer;
  signal munkzdauk : boolean_vector(4 to 0);
  signal dkpthcnsxd : integer;
  signal gmcsxyzke : boolean_vector(4 to 0);
begin
  ecq : entity work.vnbzwwz
    port map (dywtzpgma => gmcsxyzke, hwqgkzza => dkpthcnsxd);
  mdqnbqnic : entity work.vnbzwwz
    port map (dywtzpgma => munkzdauk, hwqgkzza => pbtflfoo);
  w : entity work.vnbzwwz
    port map (dywtzpgma => smoqfs, hwqgkzza => icwix);
  kkyhfpilje : entity work.vnbzwwz
    port map (dywtzpgma => yliiotz, hwqgkzza => gfczb);
  
  -- Single-driven assignments
  m <= (others => '0');
  llzivjtb <= 4 min;
end unshnigan;



-- Seed after: 14978330414870686750,10557070023141912087

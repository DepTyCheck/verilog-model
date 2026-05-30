-- Seed: 11743460954689653949,1630680796402093529



entity qzom is
  port (ewlxuby : in real_vector(3 to 0); hghblgyxld : linkage character);
end qzom;



architecture nqkcuhxnbk of qzom is
  
begin
  
end nqkcuhxnbk;



entity zgrus is
  port (xnr : inout severity_level);
end zgrus;



architecture zuqud of zgrus is
  signal dyl : real_vector(3 to 0);
  signal lnmdro : character;
  signal ikfvfu : real_vector(3 to 0);
begin
  kgyjw : entity work.qzom
    port map (ewlxuby => ikfvfu, hghblgyxld => lnmdro);
  hqni : entity work.qzom
    port map (ewlxuby => ikfvfu, hghblgyxld => lnmdro);
  vay : entity work.qzom
    port map (ewlxuby => dyl, hghblgyxld => lnmdro);
end zuqud;



entity hqrgwc is
  port (qtupcysp : out real; mxwndn : inout real_vector(3 downto 1); gpimtwr : out severity_level);
end hqrgwc;



architecture nbt of hqrgwc is
  
begin
  
end nbt;



entity hatklbex is
  port (kgqtfxsk : inout real);
end hatklbex;



architecture tvjp of hatklbex is
  signal ilayg : real_vector(3 to 0);
  signal l : character;
  signal lnd : real_vector(3 to 0);
  signal gqjxmysdo : severity_level;
begin
  khpfuclkys : entity work.zgrus
    port map (xnr => gqjxmysdo);
  tkzy : entity work.qzom
    port map (ewlxuby => lnd, hghblgyxld => l);
  yvhd : entity work.qzom
    port map (ewlxuby => ilayg, hghblgyxld => l);
end tvjp;



-- Seed after: 17737263043840056247,1630680796402093529

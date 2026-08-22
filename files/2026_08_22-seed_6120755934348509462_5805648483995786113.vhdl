-- Seed: 6120755934348509462,5805648483995786113

entity nundh is
  port (tld : inout time);
end nundh;

architecture f of nundh is
  
begin
  -- Single-driven assignments
  tld <= tld;
end f;

entity u is
  port (slmm : out real);
end u;

architecture s of u is
  signal fawfvkid : time;
  signal jnfrw : time;
begin
  oqzyk : entity work.nundh
    port map (tld => jnfrw);
  lnxxycql : entity work.nundh
    port map (tld => fawfvkid);
  
  -- Single-driven assignments
  slmm <= slmm;
end s;



-- Seed after: 6261188343453321284,5805648483995786113

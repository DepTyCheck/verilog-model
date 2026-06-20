-- Seed: 4809353829964453364,3924983747739634027

entity fsfwoqcl is
  port (codb : inout time; kx : out time; snc : buffer time);
end fsfwoqcl;

architecture iefk of fsfwoqcl is
  
begin
  -- Single-driven assignments
  snc <= 4_1_4_3_3.20 us;
  codb <= 2#11100# fs;
  kx <= 2#110.1_0# us;
end iefk;

entity bg is
  port (zhhoz : in boolean; xr : out string(5 to 2));
end bg;

architecture d of bg is
  signal cripf : time;
  signal uimcgx : time;
  signal xec : time;
  signal tzyp : time;
  signal bibvfkfap : time;
  signal kaxofjz : time;
begin
  vpcxl : entity work.fsfwoqcl
    port map (codb => kaxofjz, kx => bibvfkfap, snc => tzyp);
  gmifqsafw : entity work.fsfwoqcl
    port map (codb => xec, kx => uimcgx, snc => cripf);
  
  -- Single-driven assignments
  xr <= (others => ' ');
end d;



-- Seed after: 667469330061760444,3924983747739634027

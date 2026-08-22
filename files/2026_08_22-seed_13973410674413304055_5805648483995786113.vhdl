-- Seed: 13973410674413304055,5805648483995786113

entity uqmpi is
  port (vsw : linkage real_vector(0 downto 3); vvib : inout integer; v : buffer integer);
end uqmpi;

architecture pnfbbadiop of uqmpi is
  
begin
  -- Single-driven assignments
  v <= v;
  vvib <= v;
end pnfbbadiop;

entity g is
  port (fcasgop : buffer time);
end g;

architecture webosx of g is
  signal szlbgd : integer;
  signal bonxpmcuuc : integer;
  signal y : real_vector(0 downto 3);
  signal wcdbwfx : integer;
  signal fjfoxnylc : integer;
  signal kisdoq : real_vector(0 downto 3);
  signal mfnvhui : integer;
  signal jbamks : integer;
  signal masab : real_vector(0 downto 3);
  signal msds : integer;
  signal dlmun : integer;
  signal i : real_vector(0 downto 3);
begin
  vzwlxr : entity work.uqmpi
    port map (vsw => i, vvib => dlmun, v => msds);
  vob : entity work.uqmpi
    port map (vsw => masab, vvib => jbamks, v => mfnvhui);
  hpmorubsb : entity work.uqmpi
    port map (vsw => kisdoq, vvib => fjfoxnylc, v => wcdbwfx);
  htpi : entity work.uqmpi
    port map (vsw => y, vvib => bonxpmcuuc, v => szlbgd);
  
  -- Single-driven assignments
  fcasgop <= 2#0.0_0_1# ps;
end webosx;



-- Seed after: 13469888031087017262,5805648483995786113

-- Seed: 4158069046559323346,3108530264173481209

entity bdukpwa is
  port (oaqhkhoes : inout integer);
end bdukpwa;

architecture aqxrssg of bdukpwa is
  
begin
  -- Single-driven assignments
  oaqhkhoes <= 2#000#;
end aqxrssg;

entity xcvdleul is
  port (cxvndno : in bit);
end xcvdleul;

architecture vgxsawbtxg of xcvdleul is
  signal ytrbyxjy : integer;
  signal dprj : integer;
  signal sfx : integer;
  signal p : integer;
begin
  orlkq : entity work.bdukpwa
    port map (oaqhkhoes => p);
  ylrcfev : entity work.bdukpwa
    port map (oaqhkhoes => sfx);
  kmndfpy : entity work.bdukpwa
    port map (oaqhkhoes => dprj);
  nqask : entity work.bdukpwa
    port map (oaqhkhoes => ytrbyxjy);
end vgxsawbtxg;

entity lighqyz is
  port (druzypbqw : out time_vector(0 to 2); xn : buffer time; npif : out real_vector(1 downto 2); fpzjqww : buffer integer);
end lighqyz;

architecture fytwx of lighqyz is
  signal w : bit;
  signal ugo : integer;
begin
  nl : entity work.bdukpwa
    port map (oaqhkhoes => fpzjqww);
  sagmbp : entity work.bdukpwa
    port map (oaqhkhoes => ugo);
  jl : entity work.xcvdleul
    port map (cxvndno => w);
  
  -- Single-driven assignments
  druzypbqw <= (8#2.52# ns, 0_2_0.1_2_3_4 us, 8#4_6_2# ns);
  npif <= (others => 0.0);
end fytwx;



-- Seed after: 3115971860993751034,3108530264173481209

-- Seed: 12313145805796927535,511364357853360275

entity xus is
  port (oimdvrfrcq : linkage real; ojwul : in severity_level; bywfuwgtn : inout time; t : buffer time);
end xus;

architecture hwjssclwr of xus is
  
begin
  -- Single-driven assignments
  t <= 3011.4_3 ps;
  bywfuwgtn <= t;
end hwjssclwr;

entity u is
  port (cc : out bit_vector(3 downto 2); aoah : out bit_vector(4 downto 4));
end u;

architecture po of u is
  signal vtpgzyxx : time;
  signal ycpo : time;
  signal kuu : severity_level;
  signal aw : real;
begin
  hkgt : entity work.xus
    port map (oimdvrfrcq => aw, ojwul => kuu, bywfuwgtn => ycpo, t => vtpgzyxx);
  
  -- Single-driven assignments
  aoah <= aoah;
  cc <= ('1', '1');
end po;

entity fijrfiv is
  port (zq : in bit; m : out boolean_vector(4 downto 2); foapmzgp : linkage time);
end fijrfiv;

architecture mdyratrl of fijrfiv is
  signal vreeaff : bit_vector(4 downto 4);
  signal xqrm : bit_vector(3 downto 2);
  signal sppq : time;
  signal amlcqx : time;
  signal qti : severity_level;
  signal gqkbl : real;
begin
  nnvixa : entity work.xus
    port map (oimdvrfrcq => gqkbl, ojwul => qti, bywfuwgtn => amlcqx, t => sppq);
  dhdtjk : entity work.u
    port map (cc => xqrm, aoah => vreeaff);
  
  -- Single-driven assignments
  m <= m;
end mdyratrl;



-- Seed after: 12901077510772199521,511364357853360275

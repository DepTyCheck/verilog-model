-- Seed: 16214362566416363497,17234720251424330329

entity dmhip is
  port (ninywc : in integer);
end dmhip;

architecture qa of dmhip is
  
begin
  
end qa;

entity doeop is
  port (dm : inout time; kcwmzy : in integer_vector(1 downto 4); uij : inout integer; sy : linkage real);
end doeop;

architecture jwtvgvcff of doeop is
  
begin
  zc : entity work.dmhip
    port map (ninywc => uij);
  
  -- Single-driven assignments
  uij <= 301;
  dm <= dm;
end jwtvgvcff;

entity pjuirpm is
  port (gbugf : buffer time; lxeuqbmaeq : linkage time; fz : out real);
end pjuirpm;

architecture try of pjuirpm is
  signal xubp : integer;
  signal pekw : integer_vector(1 downto 4);
  signal e : real;
  signal eutsf : integer;
  signal qls : integer_vector(1 downto 4);
  signal qejoxtcz : time;
begin
  z : entity work.doeop
    port map (dm => qejoxtcz, kcwmzy => qls, uij => eutsf, sy => e);
  aow : entity work.doeop
    port map (dm => gbugf, kcwmzy => pekw, uij => xubp, sy => fz);
  
  -- Single-driven assignments
  qls <= qls;
  pekw <= qls;
end try;



-- Seed after: 13024890118618721375,17234720251424330329

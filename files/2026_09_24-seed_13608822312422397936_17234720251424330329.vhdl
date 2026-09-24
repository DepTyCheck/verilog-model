-- Seed: 13608822312422397936,17234720251424330329

entity jemd is
  port (ghabw : inout integer; bo : out time; ozlt : linkage integer; xnxqw : inout boolean_vector(0 downto 4));
end jemd;

architecture gpx of jemd is
  
begin
  -- Single-driven assignments
  xnxqw <= (others => TRUE);
  bo <= bo;
end gpx;

entity cmme is
  port (l : out time; rbvbeke : out time; air : buffer time);
end cmme;

architecture vfxkbyd of cmme is
  signal frq : boolean_vector(0 downto 4);
  signal krrkevpu : integer;
  signal gi : time;
  signal arlzbhu : integer;
  signal ac : boolean_vector(0 downto 4);
  signal ywm : integer;
  signal e : time;
  signal rt : integer;
  signal cmnuo : boolean_vector(0 downto 4);
  signal dxsbj : integer;
  signal u : integer;
begin
  d : entity work.jemd
    port map (ghabw => u, bo => air, ozlt => dxsbj, xnxqw => cmnuo);
  ybcldmyrsr : entity work.jemd
    port map (ghabw => rt, bo => e, ozlt => ywm, xnxqw => ac);
  cexytzg : entity work.jemd
    port map (ghabw => arlzbhu, bo => gi, ozlt => krrkevpu, xnxqw => frq);
  
  -- Single-driven assignments
  rbvbeke <= 2 sec;
  l <= 2#1# ns;
end vfxkbyd;



-- Seed after: 9735798465975711783,17234720251424330329

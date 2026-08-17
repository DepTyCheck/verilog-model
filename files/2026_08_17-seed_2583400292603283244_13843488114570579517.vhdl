-- Seed: 2583400292603283244,13843488114570579517

entity jwerezon is
  port (wjxd : inout integer_vector(1 downto 4));
end jwerezon;

architecture fhv of jwerezon is
  
begin
  -- Single-driven assignments
  wjxd <= (others => 0);
end fhv;

entity rcgrff is
  port (f : out time; hktxwgcxf : inout boolean);
end rcgrff;

architecture ak of rcgrff is
  signal lyhmvrtwe : integer_vector(1 downto 4);
begin
  nn : entity work.jwerezon
    port map (wjxd => lyhmvrtwe);
  
  -- Single-driven assignments
  hktxwgcxf <= hktxwgcxf;
  f <= f;
end ak;

entity xgykb is
  port (gnuazee : buffer time);
end xgykb;

architecture kuicq of xgykb is
  signal wpitu : integer_vector(1 downto 4);
  signal aw : integer_vector(1 downto 4);
begin
  layb : entity work.jwerezon
    port map (wjxd => aw);
  ysseufx : entity work.jwerezon
    port map (wjxd => wpitu);
end kuicq;



-- Seed after: 3689070716051088400,13843488114570579517

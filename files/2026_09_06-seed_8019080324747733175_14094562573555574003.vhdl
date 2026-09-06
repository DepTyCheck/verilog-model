-- Seed: 8019080324747733175,14094562573555574003

entity bqbish is
  port (nthenkpme : linkage integer; qf : buffer integer_vector(2 to 4); sdezhdsdtk : buffer time);
end bqbish;

architecture rkkcpl of bqbish is
  
begin
  -- Single-driven assignments
  qf <= (2#010#, 16#95BF8#, 2#11#);
end rkkcpl;

entity uv is
  port (gyjfykk : inout time; nlwpubnake : out time; kroixpma : inout character; apqjbvelli : buffer integer);
end uv;

architecture csi of uv is
  signal asabvjwai : integer_vector(2 to 4);
  signal labfxayk : integer;
  signal jmu : integer_vector(2 to 4);
  signal qcul : integer;
  signal piozp : time;
  signal fza : integer_vector(2 to 4);
  signal dz : integer;
begin
  ehc : entity work.bqbish
    port map (nthenkpme => dz, qf => fza, sdezhdsdtk => piozp);
  dgruidfyh : entity work.bqbish
    port map (nthenkpme => qcul, qf => jmu, sdezhdsdtk => nlwpubnake);
  ccrzmjg : entity work.bqbish
    port map (nthenkpme => labfxayk, qf => asabvjwai, sdezhdsdtk => gyjfykk);
  
  -- Single-driven assignments
  kroixpma <= 'i';
  apqjbvelli <= 8#6_1_0_0_3#;
end csi;



-- Seed after: 10055319208159388703,14094562573555574003

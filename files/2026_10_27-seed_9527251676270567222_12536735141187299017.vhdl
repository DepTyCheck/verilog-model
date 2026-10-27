-- Seed: 9527251676270567222,12536735141187299017

entity vjlsdtcdg is
  port (ycsosgpau : out integer);
end vjlsdtcdg;

architecture f of vjlsdtcdg is
  
begin
  -- Single-driven assignments
  ycsosgpau <= ycsosgpau;
end f;

entity nrohxagjkn is
  port (tfapfkbjf : inout real; t : inout integer);
end nrohxagjkn;

architecture reg of nrohxagjkn is
  
begin
  bur : entity work.vjlsdtcdg
    port map (ycsosgpau => t);
  
  -- Single-driven assignments
  tfapfkbjf <= 8#2_2.25#;
end reg;

entity wsmi is
  port (lzmu : buffer time);
end wsmi;

architecture ozlt of wsmi is
  
begin
  -- Single-driven assignments
  lzmu <= lzmu;
end ozlt;

entity gouxjc is
  port (yif : out time_vector(4 downto 4); lzqmt : buffer bit_vector(3 to 4));
end gouxjc;

architecture ecpgtmxfc of gouxjc is
  signal rtfbxasbo : integer;
  signal fcxbygs : integer;
  signal l : real;
begin
  ugrpkmc : entity work.nrohxagjkn
    port map (tfapfkbjf => l, t => fcxbygs);
  o : entity work.vjlsdtcdg
    port map (ycsosgpau => rtfbxasbo);
  
  -- Single-driven assignments
  lzqmt <= ('0', '1');
  yif <= (others => 8#1752# us);
end ecpgtmxfc;



-- Seed after: 7325297365253090495,12536735141187299017

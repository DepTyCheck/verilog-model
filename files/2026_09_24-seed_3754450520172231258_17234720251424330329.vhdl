-- Seed: 3754450520172231258,17234720251424330329

entity chpb is
  port (uzcmz : linkage time; r : in bit);
end chpb;

architecture ixuc of chpb is
  
begin
  
end ixuc;

entity sewmi is
  port (vngmp : buffer boolean; kkzu : buffer real; v : in bit_vector(1 to 4));
end sewmi;

architecture c of sewmi is
  signal dcjngrm : bit;
  signal frihk : time;
  signal hclrfy : bit;
  signal jxg : time;
  signal zhxpu : bit;
  signal zfbffftxua : time;
begin
  d : entity work.chpb
    port map (uzcmz => zfbffftxua, r => zhxpu);
  ieauhfz : entity work.chpb
    port map (uzcmz => jxg, r => hclrfy);
  ylgg : entity work.chpb
    port map (uzcmz => frihk, r => dcjngrm);
  
  -- Single-driven assignments
  hclrfy <= zhxpu;
  zhxpu <= '1';
  vngmp <= FALSE;
  dcjngrm <= zhxpu;
  kkzu <= 2#0_1_1_0.1011#;
end c;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (cuxh : linkage real; mnciurkx : inout real; i : out std_logic_vector(4 to 2));
end y;

architecture qc of y is
  signal rx : bit;
  signal eqgaafm : time;
  signal ayqupnpvh : bit;
  signal ljqlgllmn : time;
begin
  ywavxchn : entity work.chpb
    port map (uzcmz => ljqlgllmn, r => ayqupnpvh);
  bfllsnyra : entity work.chpb
    port map (uzcmz => eqgaafm, r => rx);
  
  -- Single-driven assignments
  rx <= '1';
  ayqupnpvh <= '1';
  mnciurkx <= mnciurkx;
  
  -- Multi-driven assignments
  i <= (others => '0');
  i <= i;
  i <= "";
end qc;

entity wimeckbh is
  port (kkrj : linkage real; sjtjfl : out bit);
end wimeckbh;

architecture ztwbrqrfgm of wimeckbh is
  signal fcupdv : time;
begin
  pbwewj : entity work.chpb
    port map (uzcmz => fcupdv, r => sjtjfl);
  
  -- Single-driven assignments
  sjtjfl <= '1';
end ztwbrqrfgm;



-- Seed after: 3839886229542003071,17234720251424330329

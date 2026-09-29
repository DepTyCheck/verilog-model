-- Seed: 6496035192440673578,10940991575366938685

entity xcafjit is
  port (dksxg : in time; wzud : in time);
end xcafjit;

architecture b of xcafjit is
  
begin
  
end b;

entity cpwzbdke is
  port (g : out time_vector(4 downto 4); ddbdnmvn : linkage bit_vector(2 downto 1));
end cpwzbdke;

architecture vwfos of cpwzbdke is
  signal lwyk : time;
  signal dxetwkduqy : time;
  signal dlmb : time;
  signal tqxs : time;
  signal ijtaizeimj : time;
  signal db : time;
begin
  pmrprvt : entity work.xcafjit
    port map (dksxg => db, wzud => ijtaizeimj);
  b : entity work.xcafjit
    port map (dksxg => tqxs, wzud => dlmb);
  jwdxjty : entity work.xcafjit
    port map (dksxg => ijtaizeimj, wzud => dxetwkduqy);
  aulzkfpcjs : entity work.xcafjit
    port map (dksxg => lwyk, wzud => dxetwkduqy);
  
  -- Single-driven assignments
  db <= db;
end vwfos;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (hgaft : in std_logic_vector(4 to 1); wveooqhxj : out std_logic_vector(1 to 4); lxbuoelr : out std_logic; wynczh : out std_logic);
end o;

architecture ttkrrpgmz of o is
  signal jfixwqfyuu : bit_vector(2 downto 1);
  signal axckfhkq : time_vector(4 downto 4);
  signal wfg : time;
  signal uk : time;
  signal brpopjdmr : bit_vector(2 downto 1);
  signal uemubq : time_vector(4 downto 4);
begin
  fmayah : entity work.cpwzbdke
    port map (g => uemubq, ddbdnmvn => brpopjdmr);
  tpzrsjuwir : entity work.xcafjit
    port map (dksxg => uk, wzud => wfg);
  ig : entity work.xcafjit
    port map (dksxg => wfg, wzud => uk);
  gnd : entity work.cpwzbdke
    port map (g => axckfhkq, ddbdnmvn => jfixwqfyuu);
  
  -- Single-driven assignments
  uk <= uk;
  wfg <= 16#D_4_C_5.E_9_B_2_6# ps;
  
  -- Multi-driven assignments
  lxbuoelr <= wynczh;
end ttkrrpgmz;



-- Seed after: 1191368486364982304,10940991575366938685

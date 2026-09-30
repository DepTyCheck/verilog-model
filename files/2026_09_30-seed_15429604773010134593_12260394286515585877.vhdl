-- Seed: 15429604773010134593,12260394286515585877

entity yqxrr is
  port (vlzjdi : linkage bit_vector(2 to 0));
end yqxrr;

architecture w of yqxrr is
  
begin
  
end w;

library ieee;
use ieee.std_logic_1164.all;

entity srzzr is
  port (cmuavynw : buffer std_logic; ttphddy : inout integer; zauxktagvk : in integer; lwksgswkw : out std_logic_vector(3 to 0));
end srzzr;

architecture lsrhtmyhs of srzzr is
  signal spttv : bit_vector(2 to 0);
begin
  ntuqhj : entity work.yqxrr
    port map (vlzjdi => spttv);
  
  -- Single-driven assignments
  ttphddy <= 22;
end lsrhtmyhs;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (sehtxnb : buffer real_vector(0 to 3); lnjczbkqy : linkage std_logic_vector(2 to 1); chtbxvn : inout real; jhybsc : buffer time);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture qkjnbpxs of n is
  signal gxlwkdatry : std_logic_vector(3 to 0);
  signal l : integer;
  signal fdlvbun : std_logic;
  signal win : bit_vector(2 to 0);
  signal jxrnycy : std_logic_vector(3 to 0);
  signal wrco : integer;
  signal avpvizk : integer;
  signal uibm : std_logic;
  signal utliqnkxg : bit_vector(2 to 0);
begin
  myunxgpch : entity work.yqxrr
    port map (vlzjdi => utliqnkxg);
  vwqpkkpng : entity work.srzzr
    port map (cmuavynw => uibm, ttphddy => avpvizk, zauxktagvk => wrco, lwksgswkw => jxrnycy);
  cdxsgheah : entity work.yqxrr
    port map (vlzjdi => win);
  ubecjrqq : entity work.srzzr
    port map (cmuavynw => fdlvbun, ttphddy => wrco, zauxktagvk => l, lwksgswkw => gxlwkdatry);
  
  -- Single-driven assignments
  jhybsc <= 2#1_0_1_1# ps;
  
  -- Multi-driven assignments
  uibm <= uibm;
end qkjnbpxs;



-- Seed after: 743936458284216051,12260394286515585877

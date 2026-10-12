-- Seed: 12053070237188097543,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity wspge is
  port (cuiys : linkage std_logic; rspesx : out integer; duyfyxvbw : in real);
end wspge;

architecture inzqtrpkjw of wspge is
  
begin
  
end inzqtrpkjw;

library ieee;
use ieee.std_logic_1164.all;

entity vrfclxkb is
  port (hi : linkage std_logic; imdw : in real);
end vrfclxkb;

architecture qxr of vrfclxkb is
  signal sa : integer;
  signal zkyc : integer;
begin
  uvmvg : entity work.wspge
    port map (cuiys => hi, rspesx => zkyc, duyfyxvbw => imdw);
  olomqq : entity work.wspge
    port map (cuiys => hi, rspesx => sa, duyfyxvbw => imdw);
end qxr;

entity dtlouxnlt is
  port (jxrjqrbwn : inout boolean; nfxsr : buffer bit);
end dtlouxnlt;

library ieee;
use ieee.std_logic_1164.all;

architecture r of dtlouxnlt is
  signal a : real;
  signal alyvqkdq : integer;
  signal pftsnhwmrq : std_logic;
  signal ecrliaktt : integer;
  signal b : std_logic;
  signal yqyyt : integer;
  signal pvlqhy : real;
  signal ovmsptqg : std_logic;
begin
  jaqxlyefo : entity work.vrfclxkb
    port map (hi => ovmsptqg, imdw => pvlqhy);
  rtgklt : entity work.wspge
    port map (cuiys => ovmsptqg, rspesx => yqyyt, duyfyxvbw => pvlqhy);
  fznxaj : entity work.wspge
    port map (cuiys => b, rspesx => ecrliaktt, duyfyxvbw => pvlqhy);
  wdvp : entity work.wspge
    port map (cuiys => pftsnhwmrq, rspesx => alyvqkdq, duyfyxvbw => a);
  
  -- Single-driven assignments
  nfxsr <= '1';
  a <= 8#5_4.51#;
  pvlqhy <= 2134.4;
  jxrjqrbwn <= FALSE;
end r;



-- Seed after: 11741455603509839653,3426943171337919827

-- Seed: 7949176258607520515,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity fmmmscbrhm is
  port ( h : linkage std_logic_vector(2 downto 1)
  ; nibdkbtn : buffer std_logic_vector(3 to 4)
  ; dtiacuysix : buffer std_logic_vector(4 downto 2)
  ; bkd : in boolean
  );
end fmmmscbrhm;

architecture ts of fmmmscbrhm is
  
begin
  -- Multi-driven assignments
  dtiacuysix <= dtiacuysix;
end ts;

entity bwkx is
  port (vs : buffer time; xhtqsuc : out boolean);
end bwkx;

library ieee;
use ieee.std_logic_1164.all;

architecture ko of bwkx is
  signal ekei : std_logic_vector(4 downto 2);
  signal bv : std_logic_vector(2 downto 1);
  signal jpzn : std_logic_vector(4 downto 2);
  signal io : std_logic_vector(2 downto 1);
  signal piuna : std_logic_vector(4 downto 2);
  signal ndrglweu : std_logic_vector(3 to 4);
  signal yml : boolean;
  signal jvrumb : std_logic_vector(4 downto 2);
  signal xwz : std_logic_vector(3 to 4);
  signal bxtzigbv : std_logic_vector(2 downto 1);
begin
  wqmm : entity work.fmmmscbrhm
    port map (h => bxtzigbv, nibdkbtn => xwz, dtiacuysix => jvrumb, bkd => yml);
  otfovfeho : entity work.fmmmscbrhm
    port map (h => bxtzigbv, nibdkbtn => ndrglweu, dtiacuysix => piuna, bkd => xhtqsuc);
  xxr : entity work.fmmmscbrhm
    port map (h => io, nibdkbtn => xwz, dtiacuysix => jpzn, bkd => xhtqsuc);
  jwy : entity work.fmmmscbrhm
    port map (h => bv, nibdkbtn => xwz, dtiacuysix => ekei, bkd => xhtqsuc);
  
  -- Multi-driven assignments
  piuna <= "0XW";
  piuna <= "XW0";
  io <= bxtzigbv;
  io <= ('L', 'L');
end ko;



-- Seed after: 16493029317628806388,1112937151005418631

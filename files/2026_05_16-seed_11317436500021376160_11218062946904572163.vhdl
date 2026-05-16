-- Seed: 11317436500021376160,11218062946904572163



entity tjrpap is
  port (syxkytlaba : linkage severity_level; ycfttno : buffer character);
end tjrpap;



architecture upodatmuz of tjrpap is
  
begin
  
end upodatmuz;

library ieee;
use ieee.std_logic_1164.all;

entity vg is
  port (whw : inout integer; dwsmy : inout std_logic; mevz : out real);
end vg;



architecture tjzuuxgogl of vg is
  signal spnyjrz : character;
  signal rfbwvnrp : severity_level;
  signal zpulv : character;
  signal zzyt : severity_level;
  signal muokhrhk : character;
  signal yrjmkzuaj : character;
  signal bykrc : severity_level;
begin
  sm : entity work.tjrpap
    port map (syxkytlaba => bykrc, ycfttno => yrjmkzuaj);
  lpzm : entity work.tjrpap
    port map (syxkytlaba => bykrc, ycfttno => muokhrhk);
  jmmon : entity work.tjrpap
    port map (syxkytlaba => zzyt, ycfttno => zpulv);
  wc : entity work.tjrpap
    port map (syxkytlaba => rfbwvnrp, ycfttno => spnyjrz);
end tjzuuxgogl;

library ieee;
use ieee.std_logic_1164.all;

entity olyuch is
  port (alndtswj : linkage real; ersgkw : out time; tikmxll : in std_logic; gbzdhsoexo : buffer std_logic);
end olyuch;



architecture daxckg of olyuch is
  signal brxgvza : character;
  signal llbnle : severity_level;
begin
  ut : entity work.tjrpap
    port map (syxkytlaba => llbnle, ycfttno => brxgvza);
end daxckg;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (rsipzhoux : inout severity_level; uxbywdbnqd : out std_logic);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture zpebxocr of v is
  signal kvjrr : character;
  signal pujymmoepw : severity_level;
  signal qncp : real;
  signal fa : integer;
  signal xb : real;
  signal slf : std_logic;
  signal r : integer;
begin
  jp : entity work.vg
    port map (whw => r, dwsmy => slf, mevz => xb);
  rizs : entity work.vg
    port map (whw => fa, dwsmy => uxbywdbnqd, mevz => qncp);
  indoye : entity work.tjrpap
    port map (syxkytlaba => pujymmoepw, ycfttno => kvjrr);
end zpebxocr;



-- Seed after: 11334343697211060225,11218062946904572163

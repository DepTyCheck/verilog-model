-- Seed: 11691881404945828181,7142793346053417159



entity ggb is
  port (qnugqvan : linkage real; lheqri : buffer time_vector(2 downto 3); r : out integer_vector(4 downto 0); b : out time);
end ggb;



architecture lqxadyd of ggb is
  
begin
  
end lqxadyd;



entity arbsk is
  port (mnnqix : out time; boxxx : linkage boolean);
end arbsk;



architecture bdiahpszao of arbsk is
  signal bfbuwoltnd : time;
  signal c : integer_vector(4 downto 0);
  signal pffgczb : time_vector(2 downto 3);
  signal dwz : integer_vector(4 downto 0);
  signal crc : time_vector(2 downto 3);
  signal cxcdyovq : real;
begin
  mgot : entity work.ggb
    port map (qnugqvan => cxcdyovq, lheqri => crc, r => dwz, b => mnnqix);
  cl : entity work.ggb
    port map (qnugqvan => cxcdyovq, lheqri => pffgczb, r => c, b => bfbuwoltnd);
end bdiahpszao;

library ieee;
use ieee.std_logic_1164.all;

entity aihfccsryy is
  port (ujnfks : out std_logic; pfhssvv : buffer bit; nrghqh : out integer);
end aihfccsryy;



architecture t of aihfccsryy is
  signal nmos : boolean;
  signal wtacyqg : time;
  signal xpbgsvmrr : boolean;
  signal xjfyaytjnc : time;
  signal txmta : time;
  signal ruxqjkz : integer_vector(4 downto 0);
  signal wgaviq : time_vector(2 downto 3);
  signal wmsujow : real;
  signal cbolhxn : time;
  signal yenbqowc : integer_vector(4 downto 0);
  signal tnbavmzlm : time_vector(2 downto 3);
  signal oytyw : real;
begin
  gktp : entity work.ggb
    port map (qnugqvan => oytyw, lheqri => tnbavmzlm, r => yenbqowc, b => cbolhxn);
  gvc : entity work.ggb
    port map (qnugqvan => wmsujow, lheqri => wgaviq, r => ruxqjkz, b => txmta);
  ttghxet : entity work.arbsk
    port map (mnnqix => xjfyaytjnc, boxxx => xpbgsvmrr);
  rcgislbx : entity work.arbsk
    port map (mnnqix => wtacyqg, boxxx => nmos);
end t;



-- Seed after: 5305641964929364135,7142793346053417159

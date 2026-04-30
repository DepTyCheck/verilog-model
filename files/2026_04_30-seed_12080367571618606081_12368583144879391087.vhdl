-- Seed: 12080367571618606081,12368583144879391087

library ieee;
use ieee.std_logic_1164.all;

entity dtm is
  port (gysoehy : buffer time; emrovbgvkq : in std_logic; d : in real; xy : out real);
end dtm;



architecture ynz of dtm is
  
begin
  
end ynz;



entity bapywkprp is
  port (vj : buffer time);
end bapywkprp;

library ieee;
use ieee.std_logic_1164.all;

architecture tbrgoo of bapywkprp is
  signal nzlu : time;
  signal xsbeycyc : real;
  signal qs : real;
  signal ttgbfaiqnm : std_logic;
  signal z : time;
begin
  d : entity work.dtm
    port map (gysoehy => z, emrovbgvkq => ttgbfaiqnm, d => qs, xy => xsbeycyc);
  buaj : entity work.dtm
    port map (gysoehy => nzlu, emrovbgvkq => ttgbfaiqnm, d => qs, xy => qs);
end tbrgoo;



entity q is
  port (reyozoew : buffer real);
end q;

library ieee;
use ieee.std_logic_1164.all;

architecture zw of q is
  signal ndfebskqte : real;
  signal gfqydf : time;
  signal kgpw : real;
  signal xhspaapns : time;
  signal asmueksn : real;
  signal ubf : std_logic;
  signal pnimszhnh : time;
  signal wkd : time;
begin
  strnlrkjth : entity work.bapywkprp
    port map (vj => wkd);
  rfdsex : entity work.dtm
    port map (gysoehy => pnimszhnh, emrovbgvkq => ubf, d => asmueksn, xy => reyozoew);
  qqgvp : entity work.dtm
    port map (gysoehy => xhspaapns, emrovbgvkq => ubf, d => kgpw, xy => asmueksn);
  ltoqdad : entity work.dtm
    port map (gysoehy => gfqydf, emrovbgvkq => ubf, d => asmueksn, xy => ndfebskqte);
end zw;



-- Seed after: 9258765978499585288,12368583144879391087

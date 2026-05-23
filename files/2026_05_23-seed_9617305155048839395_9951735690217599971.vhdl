-- Seed: 9617305155048839395,9951735690217599971



entity nzlej is
  port (nfrw : linkage character; ri : linkage integer; fxvhxyieey : linkage bit_vector(4 downto 0); pmwzss : buffer integer);
end nzlej;



architecture auzkgzcgh of nzlej is
  
begin
  
end auzkgzcgh;



entity osxx is
  port (bwhxwr : in integer);
end osxx;



architecture i of osxx is
  signal xoudhh : integer;
  signal cmwprc : integer;
  signal kbcrtnla : character;
  signal s : integer;
  signal uhga : integer;
  signal ulzwqjp : character;
  signal kzkhnbetaa : integer;
  signal rkdnsgctiw : bit_vector(4 downto 0);
  signal zsg : character;
begin
  sh : entity work.nzlej
    port map (nfrw => zsg, ri => bwhxwr, fxvhxyieey => rkdnsgctiw, pmwzss => kzkhnbetaa);
  vhih : entity work.nzlej
    port map (nfrw => ulzwqjp, ri => uhga, fxvhxyieey => rkdnsgctiw, pmwzss => s);
  qzwasp : entity work.nzlej
    port map (nfrw => kbcrtnla, ri => kzkhnbetaa, fxvhxyieey => rkdnsgctiw, pmwzss => cmwprc);
  fuoxvgy : entity work.nzlej
    port map (nfrw => zsg, ri => bwhxwr, fxvhxyieey => rkdnsgctiw, pmwzss => xoudhh);
end i;

library ieee;
use ieee.std_logic_1164.all;

entity man is
  port (dii : out std_logic_vector(4 downto 2));
end man;



architecture otjcyteaxq of man is
  signal chmzmwn : integer;
  signal voynrbfi : bit_vector(4 downto 0);
  signal gyfwcyu : integer;
  signal p : character;
  signal caxzt : integer;
begin
  euhvzs : entity work.osxx
    port map (bwhxwr => caxzt);
  hebxx : entity work.nzlej
    port map (nfrw => p, ri => gyfwcyu, fxvhxyieey => voynrbfi, pmwzss => chmzmwn);
end otjcyteaxq;



entity vm is
  port (jzqigdxvo : linkage character; qhpzrhsd : out integer_vector(3 to 2); dunv : in time);
end vm;

library ieee;
use ieee.std_logic_1164.all;

architecture pi of vm is
  signal qjtu : integer;
  signal bgfofrylk : std_logic_vector(4 downto 2);
begin
  evfgxdwd : entity work.man
    port map (dii => bgfofrylk);
  ypzwyd : entity work.osxx
    port map (bwhxwr => qjtu);
end pi;



-- Seed after: 17690013328243438552,9951735690217599971

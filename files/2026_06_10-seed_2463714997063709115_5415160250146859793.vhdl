-- Seed: 2463714997063709115,5415160250146859793



entity oy is
  port (scxzhtxuc : linkage real; vgn : buffer integer_vector(4 downto 0); frektx : out time; pkkvlzydc : in real);
end oy;



architecture w of oy is
  
begin
  
end w;

library ieee;
use ieee.std_logic_1164.all;

entity jmgdnh is
  port (ximgltiaco : inout std_logic; pjorhyh : buffer real);
end jmgdnh;



architecture bhalib of jmgdnh is
  signal tyokal : real;
  signal ypablsleai : time;
  signal tnhupzp : integer_vector(4 downto 0);
  signal tmx : time;
  signal quxbq : integer_vector(4 downto 0);
  signal gtzbfhrsa : real;
begin
  v : entity work.oy
    port map (scxzhtxuc => gtzbfhrsa, vgn => quxbq, frektx => tmx, pkkvlzydc => pjorhyh);
  qstybxfg : entity work.oy
    port map (scxzhtxuc => pjorhyh, vgn => tnhupzp, frektx => ypablsleai, pkkvlzydc => tyokal);
end bhalib;



entity gmhqygbauu is
  port (iaafvtpb : out real);
end gmhqygbauu;

library ieee;
use ieee.std_logic_1164.all;

architecture isuuqrqp of gmhqygbauu is
  signal hgbdcqg : time;
  signal nsejbzhoep : integer_vector(4 downto 0);
  signal vujaov : real;
  signal bhntdp : real;
  signal xrycepzxaj : std_logic;
  signal zvn : real;
  signal cfpjvypc : time;
  signal khml : integer_vector(4 downto 0);
  signal niqk : real;
  signal yovg : time;
  signal evcs : integer_vector(4 downto 0);
begin
  rlhervst : entity work.oy
    port map (scxzhtxuc => iaafvtpb, vgn => evcs, frektx => yovg, pkkvlzydc => iaafvtpb);
  tl : entity work.oy
    port map (scxzhtxuc => niqk, vgn => khml, frektx => cfpjvypc, pkkvlzydc => zvn);
  ewnzxvukr : entity work.jmgdnh
    port map (ximgltiaco => xrycepzxaj, pjorhyh => bhntdp);
  aygsuanphc : entity work.oy
    port map (scxzhtxuc => vujaov, vgn => nsejbzhoep, frektx => hgbdcqg, pkkvlzydc => niqk);
end isuuqrqp;



-- Seed after: 13512011537559042284,5415160250146859793

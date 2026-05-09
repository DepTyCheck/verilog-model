-- Seed: 12549559919143575579,16716360695494742805



entity pligfizua is
  port (tapzivwtk : out time; t : out integer);
end pligfizua;



architecture mld of pligfizua is
  
begin
  
end mld;



entity td is
  port (mq : out real; m : linkage boolean; zrkd : in real);
end td;



architecture c of td is
  signal fcananpuyx : integer;
  signal ruzwptkic : time;
  signal ixdr : integer;
  signal rbuysgwxz : time;
begin
  odugcu : entity work.pligfizua
    port map (tapzivwtk => rbuysgwxz, t => ixdr);
  x : entity work.pligfizua
    port map (tapzivwtk => ruzwptkic, t => fcananpuyx);
end c;

library ieee;
use ieee.std_logic_1164.all;

entity miz is
  port (xhwztonk : out time; l : in character; tj : buffer std_logic; jajq : buffer real);
end miz;



architecture mhz of miz is
  signal bhyzsvrjof : real;
  signal rybxdbmhab : boolean;
begin
  jx : entity work.td
    port map (mq => jajq, m => rybxdbmhab, zrkd => bhyzsvrjof);
end mhz;

library ieee;
use ieee.std_logic_1164.all;

entity wcxzhu is
  port (tusecscb : out std_logic);
end wcxzhu;



architecture cptfufgm of wcxzhu is
  signal q : integer;
  signal fyuyx : time;
  signal jmaz : real;
  signal jbwf : boolean;
  signal rit : real;
  signal mgocyjshc : boolean;
  signal ulipzv : real;
begin
  xa : entity work.td
    port map (mq => ulipzv, m => mgocyjshc, zrkd => ulipzv);
  purrpyyp : entity work.td
    port map (mq => rit, m => jbwf, zrkd => jmaz);
  bgoxr : entity work.pligfizua
    port map (tapzivwtk => fyuyx, t => q);
end cptfufgm;



-- Seed after: 1869892554798602124,16716360695494742805

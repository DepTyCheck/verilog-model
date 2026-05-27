-- Seed: 1865763938932882627,13854332967471039201



entity xykfmh is
  port (ljhv : linkage real; aidls : out time; zaaev : linkage time; cqfmkqhdj : linkage real);
end xykfmh;



architecture cno of xykfmh is
  
begin
  
end cno;

library ieee;
use ieee.std_logic_1164.all;

entity njzxxd is
  port (inard : inout std_logic; hdd : inout real; hv : linkage real; zbrrxiv : inout boolean);
end njzxxd;



architecture cd of njzxxd is
  signal jigklpmmkx : real;
  signal xiftml : time;
  signal dc : real;
  signal igr : time;
  signal qdkidcthtg : time;
  signal scztlxeny : time;
begin
  o : entity work.xykfmh
    port map (ljhv => hv, aidls => scztlxeny, zaaev => qdkidcthtg, cqfmkqhdj => hv);
  dh : entity work.xykfmh
    port map (ljhv => hv, aidls => qdkidcthtg, zaaev => igr, cqfmkqhdj => dc);
  tzzqps : entity work.xykfmh
    port map (ljhv => hv, aidls => igr, zaaev => xiftml, cqfmkqhdj => jigklpmmkx);
end cd;

library ieee;
use ieee.std_logic_1164.all;

entity tlzpoqvvt is
  port (ofyovkkt : buffer std_logic; rjnmjy : inout time; iahdy : in boolean_vector(0 to 3); ce : in time);
end tlzpoqvvt;



architecture torkkfca of tlzpoqvvt is
  signal ztg : real;
  signal dttfcchiv : time;
  signal uxytb : time;
  signal rtckpc : real;
begin
  b : entity work.xykfmh
    port map (ljhv => rtckpc, aidls => uxytb, zaaev => dttfcchiv, cqfmkqhdj => ztg);
end torkkfca;

library ieee;
use ieee.std_logic_1164.all;

entity dsos is
  port (imuxqxf : inout std_logic_vector(1 downto 2); akagnv : linkage integer; ptyhhajpzx : out time);
end dsos;

library ieee;
use ieee.std_logic_1164.all;

architecture utuvwty of dsos is
  signal iyzbzady : real;
  signal h : time;
  signal gpxzoe : boolean;
  signal g : real;
  signal eqcees : real;
  signal gbnqvt : std_logic;
  signal sazamyqg : boolean_vector(0 to 3);
  signal fj : time;
  signal xxwn : std_logic;
begin
  yut : entity work.tlzpoqvvt
    port map (ofyovkkt => xxwn, rjnmjy => fj, iahdy => sazamyqg, ce => ptyhhajpzx);
  lolshxnp : entity work.njzxxd
    port map (inard => gbnqvt, hdd => eqcees, hv => g, zbrrxiv => gpxzoe);
  lsaiggys : entity work.xykfmh
    port map (ljhv => eqcees, aidls => ptyhhajpzx, zaaev => h, cqfmkqhdj => iyzbzady);
end utuvwty;



-- Seed after: 5465357889203115458,13854332967471039201

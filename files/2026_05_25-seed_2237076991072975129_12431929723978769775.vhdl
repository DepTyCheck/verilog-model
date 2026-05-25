-- Seed: 2237076991072975129,12431929723978769775



entity fnc is
  port (sdymkagf : in integer_vector(4 to 2));
end fnc;



architecture c of fnc is
  
begin
  
end c;



entity mdaznndppu is
  port (cpjdop : in real; ihixt : buffer integer);
end mdaznndppu;



architecture biiafzxv of mdaznndppu is
  signal nzoonr : integer_vector(4 to 2);
  signal zmuulfhql : integer_vector(4 to 2);
begin
  rvkmvww : entity work.fnc
    port map (sdymkagf => zmuulfhql);
  bjy : entity work.fnc
    port map (sdymkagf => nzoonr);
end biiafzxv;

library ieee;
use ieee.std_logic_1164.all;

entity fiijqzksta is
  port (gyul : inout time; epupr : buffer std_logic_vector(0 to 3); rnmgciagt : linkage std_logic; eiqmgcti : out std_logic);
end fiijqzksta;



architecture mdvwd of fiijqzksta is
  signal swi : integer_vector(4 to 2);
  signal kk : integer;
  signal xopupea : real;
  signal y : integer_vector(4 to 2);
begin
  kg : entity work.fnc
    port map (sdymkagf => y);
  dnx : entity work.mdaznndppu
    port map (cpjdop => xopupea, ihixt => kk);
  vlhiuldzye : entity work.fnc
    port map (sdymkagf => swi);
end mdvwd;

library ieee;
use ieee.std_logic_1164.all;

entity ybzzvwrsct is
  port (ucrzamn : linkage time; sth : out std_logic; vunbddqqv : linkage std_logic; bmfkru : out std_logic_vector(3 to 4));
end ybzzvwrsct;

library ieee;
use ieee.std_logic_1164.all;

architecture zzb of ybzzvwrsct is
  signal ylhmuctesv : std_logic;
  signal a : std_logic_vector(0 to 3);
  signal wjyoyqvx : time;
  signal arnxin : std_logic;
  signal gdrrujvapy : std_logic_vector(0 to 3);
  signal lx : time;
  signal qry : integer_vector(4 to 2);
begin
  xdk : entity work.fnc
    port map (sdymkagf => qry);
  dmrtdwvbkf : entity work.fiijqzksta
    port map (gyul => lx, epupr => gdrrujvapy, rnmgciagt => vunbddqqv, eiqmgcti => arnxin);
  rehpkqogqa : entity work.fiijqzksta
    port map (gyul => wjyoyqvx, epupr => a, rnmgciagt => ylhmuctesv, eiqmgcti => sth);
  xx : entity work.fnc
    port map (sdymkagf => qry);
end zzb;



-- Seed after: 4150352552291649009,12431929723978769775

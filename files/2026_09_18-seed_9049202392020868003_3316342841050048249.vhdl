-- Seed: 9049202392020868003,3316342841050048249

entity basaxretjk is
  port (pyk : buffer boolean_vector(1 to 4); xfigjtxcbz : linkage character; ijnujv : inout real; asqoghpm : out time);
end basaxretjk;

architecture wutzhsh of basaxretjk is
  
begin
  
end wutzhsh;

entity s is
  port (ggbq : in time; ceix : in real);
end s;

architecture otnwfqz of s is
  signal wvc : time;
  signal eubsozeu : real;
  signal xa : character;
  signal dcppkwtsvj : boolean_vector(1 to 4);
  signal ht : time;
  signal mfbkl : real;
  signal jkzainfuj : character;
  signal gar : boolean_vector(1 to 4);
  signal xeyxulpwjm : time;
  signal hrjfkiw : real;
  signal qqrsz : character;
  signal nray : boolean_vector(1 to 4);
  signal lahpagvv : time;
  signal askze : real;
  signal ibra : character;
  signal zdx : boolean_vector(1 to 4);
begin
  oq : entity work.basaxretjk
    port map (pyk => zdx, xfigjtxcbz => ibra, ijnujv => askze, asqoghpm => lahpagvv);
  xtsne : entity work.basaxretjk
    port map (pyk => nray, xfigjtxcbz => qqrsz, ijnujv => hrjfkiw, asqoghpm => xeyxulpwjm);
  nv : entity work.basaxretjk
    port map (pyk => gar, xfigjtxcbz => jkzainfuj, ijnujv => mfbkl, asqoghpm => ht);
  svnsh : entity work.basaxretjk
    port map (pyk => dcppkwtsvj, xfigjtxcbz => xa, ijnujv => eubsozeu, asqoghpm => wvc);
end otnwfqz;

library ieee;
use ieee.std_logic_1164.all;

entity kvirz is
  port (gtiyuh : in time; u : buffer std_logic);
end kvirz;

architecture awevdpw of kvirz is
  signal dlksrmiegn : real;
begin
  untgfog : entity work.s
    port map (ggbq => gtiyuh, ceix => dlksrmiegn);
  
  -- Single-driven assignments
  dlksrmiegn <= dlksrmiegn;
  
  -- Multi-driven assignments
  u <= u;
end awevdpw;



-- Seed after: 5256915772873460294,3316342841050048249

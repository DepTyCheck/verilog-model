-- Seed: 407404923160018538,16715549879197889543



entity kytpzm is
  port (t : inout real; jxn : out time; hbqwihpe : buffer integer);
end kytpzm;



architecture tsgggu of kytpzm is
  
begin
  
end tsgggu;

library ieee;
use ieee.std_logic_1164.all;

entity znh is
  port (t : out boolean; cmmvtqdav : linkage std_logic_vector(4 downto 0); kaxs : in boolean);
end znh;



architecture ykliroqfo of znh is
  signal leatuvc : integer;
  signal mtjbo : time;
  signal ypuojl : real;
begin
  jjwcn : entity work.kytpzm
    port map (t => ypuojl, jxn => mtjbo, hbqwihpe => leatuvc);
end ykliroqfo;



entity anjxa is
  port (okn : buffer time);
end anjxa;

library ieee;
use ieee.std_logic_1164.all;

architecture cynibugyzu of anjxa is
  signal w : integer;
  signal mhxycqkoax : real;
  signal xqglieloyf : std_logic_vector(4 downto 0);
  signal nwnbggbdhw : boolean;
  signal vwhter : integer;
  signal dbs : time;
  signal b : real;
  signal gxcotdzqdp : boolean;
  signal gktt : std_logic_vector(4 downto 0);
  signal scs : boolean;
begin
  rhseyrw : entity work.znh
    port map (t => scs, cmmvtqdav => gktt, kaxs => gxcotdzqdp);
  vzxyj : entity work.kytpzm
    port map (t => b, jxn => dbs, hbqwihpe => vwhter);
  pguksory : entity work.znh
    port map (t => nwnbggbdhw, cmmvtqdav => xqglieloyf, kaxs => nwnbggbdhw);
  bjpl : entity work.kytpzm
    port map (t => mhxycqkoax, jxn => okn, hbqwihpe => w);
end cynibugyzu;



-- Seed after: 6147784155668040542,16715549879197889543

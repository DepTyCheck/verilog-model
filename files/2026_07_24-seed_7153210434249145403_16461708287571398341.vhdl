-- Seed: 7153210434249145403,16461708287571398341

entity votaezta is
  port (pzol : inout boolean; stuhsjlaay : out bit_vector(4 downto 2));
end votaezta;

architecture ogepgn of votaezta is
  
begin
  
end ogepgn;

entity llbldd is
  port (pgyphw : in time_vector(1 to 4); a : in boolean_vector(2 downto 3); pk : linkage integer; gx : in real);
end llbldd;

architecture xhhkbtpb of llbldd is
  signal kynspu : bit_vector(4 downto 2);
  signal awbyjb : boolean;
  signal i : bit_vector(4 downto 2);
  signal pattwtz : boolean;
  signal v : bit_vector(4 downto 2);
  signal ouhulghyw : boolean;
  signal kazgc : bit_vector(4 downto 2);
  signal sxgqcz : boolean;
begin
  qkjatyvys : entity work.votaezta
    port map (pzol => sxgqcz, stuhsjlaay => kazgc);
  o : entity work.votaezta
    port map (pzol => ouhulghyw, stuhsjlaay => v);
  pzgcc : entity work.votaezta
    port map (pzol => pattwtz, stuhsjlaay => i);
  fb : entity work.votaezta
    port map (pzol => awbyjb, stuhsjlaay => kynspu);
end xhhkbtpb;

library ieee;
use ieee.std_logic_1164.all;

entity ykw is
  port (gyavj : linkage std_logic_vector(0 to 3); hqmxyma : buffer time; tjmougps : linkage real);
end ykw;

architecture dnuccccyrr of ykw is
  
begin
  -- Single-driven assignments
  hqmxyma <= 11 ps;
end dnuccccyrr;

entity ubgmp is
  port (cfczfxae : linkage integer);
end ubgmp;

library ieee;
use ieee.std_logic_1164.all;

architecture xdhmi of ubgmp is
  signal qltxbrt : real;
  signal mlckji : time;
  signal lk : real;
  signal hewqj : time;
  signal yw : std_logic_vector(0 to 3);
begin
  jybtxwiw : entity work.ykw
    port map (gyavj => yw, hqmxyma => hewqj, tjmougps => lk);
  chsusaknbz : entity work.ykw
    port map (gyavj => yw, hqmxyma => mlckji, tjmougps => qltxbrt);
  
  -- Multi-driven assignments
  yw <= yw;
  yw <= "LH0X";
  yw <= yw;
end xdhmi;



-- Seed after: 14065062363689365470,16461708287571398341

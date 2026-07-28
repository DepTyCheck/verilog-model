-- Seed: 8963318928613157393,2511821214772927453

entity sn is
  port (nb : out time; oqvpbxz : inout integer; xjxacecyv : buffer boolean_vector(3 to 0));
end sn;

architecture ykjspnu of sn is
  
begin
  -- Single-driven assignments
  xjxacecyv <= xjxacecyv;
  nb <= 13.20 ps;
  oqvpbxz <= 2#1#;
end ykjspnu;

library ieee;
use ieee.std_logic_1164.all;

entity euh is
  port (zdmnqp : inout integer; vtpmhe : out integer; ihvkgkoqp : out std_logic);
end euh;

architecture wydaehi of euh is
  
begin
  -- Single-driven assignments
  vtpmhe <= vtpmhe;
  
  -- Multi-driven assignments
  ihvkgkoqp <= ihvkgkoqp;
  ihvkgkoqp <= 'X';
  ihvkgkoqp <= ihvkgkoqp;
  ihvkgkoqp <= ihvkgkoqp;
end wydaehi;

entity dvhdxebsyh is
  port (vovlthkwbn : out time);
end dvhdxebsyh;

library ieee;
use ieee.std_logic_1164.all;

architecture x of dvhdxebsyh is
  signal p : std_logic;
  signal jyiqi : integer;
  signal miki : integer;
  signal roks : boolean_vector(3 to 0);
  signal cwm : integer;
  signal joktutz : boolean_vector(3 to 0);
  signal lvrslunbj : integer;
  signal ip : time;
  signal hlaryceda : std_logic;
  signal shvkjb : integer;
  signal sgfmprpxiz : integer;
begin
  g : entity work.euh
    port map (zdmnqp => sgfmprpxiz, vtpmhe => shvkjb, ihvkgkoqp => hlaryceda);
  gkoklljomf : entity work.sn
    port map (nb => ip, oqvpbxz => lvrslunbj, xjxacecyv => joktutz);
  gzxvov : entity work.sn
    port map (nb => vovlthkwbn, oqvpbxz => cwm, xjxacecyv => roks);
  pqcnlwy : entity work.euh
    port map (zdmnqp => miki, vtpmhe => jyiqi, ihvkgkoqp => p);
end x;



-- Seed after: 10548431998587272580,2511821214772927453

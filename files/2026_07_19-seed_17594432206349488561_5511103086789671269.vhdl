-- Seed: 17594432206349488561,5511103086789671269

entity xcs is
  port (xbwluflrp : linkage real; fpeqnnsrbb : buffer time; pwbehuj : linkage boolean_vector(1 to 1));
end xcs;

architecture qtuukrc of xcs is
  
begin
  -- Single-driven assignments
  fpeqnnsrbb <= fpeqnnsrbb;
end qtuukrc;

entity bsjmnagqcy is
  port (zjruy : inout real; jsarclczp : buffer integer; huy : buffer time);
end bsjmnagqcy;

architecture uvnmfxa of bsjmnagqcy is
  signal sajsgamg : boolean_vector(1 to 1);
begin
  tt : entity work.xcs
    port map (xbwluflrp => zjruy, fpeqnnsrbb => huy, pwbehuj => sajsgamg);
  
  -- Single-driven assignments
  jsarclczp <= 16#0#;
end uvnmfxa;

library ieee;
use ieee.std_logic_1164.all;

entity kowagec is
  port (xwo : inout std_logic);
end kowagec;

architecture qd of kowagec is
  signal e : boolean_vector(1 to 1);
  signal fmppqgp : time;
  signal rdtrebgqff : real;
  signal pznnd : boolean_vector(1 to 1);
  signal qk : time;
  signal rzbyt : real;
  signal xvxarg : time;
  signal ustka : integer;
  signal b : real;
  signal lchze : time;
  signal yhgfodym : integer;
  signal pbzmphjge : real;
begin
  jlvrguztwj : entity work.bsjmnagqcy
    port map (zjruy => pbzmphjge, jsarclczp => yhgfodym, huy => lchze);
  rmckvkg : entity work.bsjmnagqcy
    port map (zjruy => b, jsarclczp => ustka, huy => xvxarg);
  iwdfk : entity work.xcs
    port map (xbwluflrp => rzbyt, fpeqnnsrbb => qk, pwbehuj => pznnd);
  ikzq : entity work.xcs
    port map (xbwluflrp => rdtrebgqff, fpeqnnsrbb => fmppqgp, pwbehuj => e);
end qd;

library ieee;
use ieee.std_logic_1164.all;

entity tiwiheaz is
  port (ppcusxown : out std_logic_vector(1 downto 2));
end tiwiheaz;

architecture ftz of tiwiheaz is
  
begin
  -- Multi-driven assignments
  ppcusxown <= (others => '0');
  ppcusxown <= ppcusxown;
  ppcusxown <= "";
  ppcusxown <= ppcusxown;
end ftz;



-- Seed after: 16517071754426560446,5511103086789671269

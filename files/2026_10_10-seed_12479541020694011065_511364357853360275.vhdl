-- Seed: 12479541020694011065,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity tjry is
  port (ndou : buffer string(4 downto 4); dcjd : in real; oc : in real; grbdxyml : buffer std_logic);
end tjry;

architecture tf of tjry is
  
begin
  -- Single-driven assignments
  ndou <= (others => 'p');
  
  -- Multi-driven assignments
  grbdxyml <= 'W';
  grbdxyml <= '1';
end tf;

entity iuei is
  port (djkpkx : in boolean_vector(0 downto 2); aurtyog : inout integer; gxcrjrmhmu : buffer boolean_vector(2 to 4); lnjtmpzvyu : inout severity_level);
end iuei;

library ieee;
use ieee.std_logic_1164.all;

architecture lznxmyfo of iuei is
  signal ecwwsicyf : std_logic;
  signal ffrlo : real;
  signal yxsdkipw : real;
  signal wq : string(4 downto 4);
  signal jczr : std_logic;
  signal lyxdafra : real;
  signal xmkyqidst : string(4 downto 4);
begin
  twxpr : entity work.tjry
    port map (ndou => xmkyqidst, dcjd => lyxdafra, oc => lyxdafra, grbdxyml => jczr);
  hhmrwu : entity work.tjry
    port map (ndou => wq, dcjd => yxsdkipw, oc => ffrlo, grbdxyml => ecwwsicyf);
  
  -- Single-driven assignments
  aurtyog <= aurtyog;
  yxsdkipw <= 16#E_F_B_4.0_A_6_B#;
  lnjtmpzvyu <= lnjtmpzvyu;
  gxcrjrmhmu <= (TRUE, TRUE, FALSE);
  
  -- Multi-driven assignments
  ecwwsicyf <= '1';
  jczr <= 'H';
end lznxmyfo;



-- Seed after: 68049588332310724,511364357853360275

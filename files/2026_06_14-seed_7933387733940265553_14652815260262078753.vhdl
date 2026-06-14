-- Seed: 7933387733940265553,14652815260262078753

entity iqcqrfbc is
  port (thjoxhzzft : buffer real_vector(3 downto 0));
end iqcqrfbc;

architecture re of iqcqrfbc is
  
begin
  
end re;

library ieee;
use ieee.std_logic_1164.all;

entity dhxgigevh is
  port (ixw : out std_logic; v : buffer bit; tziigppnu : inout std_logic_vector(2 downto 1));
end dhxgigevh;

architecture bwmnqljh of dhxgigevh is
  signal vf : real_vector(3 downto 0);
  signal jv : real_vector(3 downto 0);
  signal cmhbgav : real_vector(3 downto 0);
  signal clpdbxd : real_vector(3 downto 0);
begin
  dsqlpzk : entity work.iqcqrfbc
    port map (thjoxhzzft => clpdbxd);
  hkqlqgqp : entity work.iqcqrfbc
    port map (thjoxhzzft => cmhbgav);
  iajrzzr : entity work.iqcqrfbc
    port map (thjoxhzzft => jv);
  gtlrbio : entity work.iqcqrfbc
    port map (thjoxhzzft => vf);
  
  -- Single-driven assignments
  v <= '0';
end bwmnqljh;

entity ivzdofkc is
  port (efjeqs : inout real; pjb : out integer; whpsrdyfg : in time);
end ivzdofkc;

library ieee;
use ieee.std_logic_1164.all;

architecture yyuv of ivzdofkc is
  signal lbaeqes : std_logic_vector(2 downto 1);
  signal mf : bit;
  signal fnbygwjvxu : std_logic;
  signal tlxvjdix : real_vector(3 downto 0);
begin
  v : entity work.iqcqrfbc
    port map (thjoxhzzft => tlxvjdix);
  wa : entity work.dhxgigevh
    port map (ixw => fnbygwjvxu, v => mf, tziigppnu => lbaeqes);
  
  -- Single-driven assignments
  efjeqs <= 30.21;
  pjb <= 16#E_A_F_8_B#;
  
  -- Multi-driven assignments
  lbaeqes <= ('1', '0');
  fnbygwjvxu <= 'Z';
  fnbygwjvxu <= '0';
  lbaeqes <= "1L";
end yyuv;

entity kxosfcdyfn is
  port (ijdcsd : out character; wznbndt : out severity_level);
end kxosfcdyfn;

architecture sa of kxosfcdyfn is
  signal rou : real_vector(3 downto 0);
  signal qtbodkezbk : real_vector(3 downto 0);
  signal ek : time;
  signal nrsntgu : integer;
  signal zzqftwxn : real;
  signal nto : real_vector(3 downto 0);
begin
  pxlbpmf : entity work.iqcqrfbc
    port map (thjoxhzzft => nto);
  ggfkokcgmx : entity work.ivzdofkc
    port map (efjeqs => zzqftwxn, pjb => nrsntgu, whpsrdyfg => ek);
  gqf : entity work.iqcqrfbc
    port map (thjoxhzzft => qtbodkezbk);
  fjsiwh : entity work.iqcqrfbc
    port map (thjoxhzzft => rou);
  
  -- Single-driven assignments
  wznbndt <= ERROR;
  ijdcsd <= 'k';
end sa;



-- Seed after: 339465977830821294,14652815260262078753

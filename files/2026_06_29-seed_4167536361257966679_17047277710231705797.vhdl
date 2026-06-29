-- Seed: 4167536361257966679,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity dn is
  port ( syto : linkage std_logic_vector(2 to 4)
  ; toejtdr : out std_logic_vector(1 downto 0)
  ; ktryurphbu : in boolean_vector(0 to 2)
  ; nfoycxmv : buffer real_vector(2 downto 0)
  );
end dn;

architecture gur of dn is
  
begin
  -- Single-driven assignments
  nfoycxmv <= (2.2_1_1_1_4, 2#1_0_0.001#, 0_0.3_0_1_0);
  
  -- Multi-driven assignments
  toejtdr <= ('H', '-');
end gur;

entity fiwac is
  port (zs : linkage character; ybdpgh : linkage character);
end fiwac;

library ieee;
use ieee.std_logic_1164.all;

architecture hywzgnv of fiwac is
  signal uhu : real_vector(2 downto 0);
  signal onjtcvrj : boolean_vector(0 to 2);
  signal qwsp : std_logic_vector(1 downto 0);
  signal upuf : std_logic_vector(2 to 4);
begin
  hao : entity work.dn
    port map (syto => upuf, toejtdr => qwsp, ktryurphbu => onjtcvrj, nfoycxmv => uhu);
  
  -- Single-driven assignments
  onjtcvrj <= (FALSE, TRUE, FALSE);
  
  -- Multi-driven assignments
  upuf <= "HLW";
  qwsp <= ('0', 'Z');
  qwsp <= "Z0";
end hywzgnv;

entity gohecqgcqn is
  port (vszytxo : buffer character; itolvk : buffer bit_vector(0 to 3); l : inout string(4 to 5));
end gohecqgcqn;

library ieee;
use ieee.std_logic_1164.all;

architecture h of gohecqgcqn is
  signal uobp : real_vector(2 downto 0);
  signal rdvwx : boolean_vector(0 to 2);
  signal bdktlhbizm : std_logic_vector(1 downto 0);
  signal qbkwrj : real_vector(2 downto 0);
  signal sxq : boolean_vector(0 to 2);
  signal olvoag : std_logic_vector(1 downto 0);
  signal pu : std_logic_vector(2 to 4);
  signal j : character;
  signal fh : character;
begin
  gyt : entity work.fiwac
    port map (zs => fh, ybdpgh => j);
  diulvska : entity work.dn
    port map (syto => pu, toejtdr => olvoag, ktryurphbu => sxq, nfoycxmv => qbkwrj);
  lkttknycde : entity work.dn
    port map (syto => pu, toejtdr => bdktlhbizm, ktryurphbu => rdvwx, nfoycxmv => uobp);
  
  -- Single-driven assignments
  vszytxo <= 'r';
  l <= "vu";
  
  -- Multi-driven assignments
  pu <= "101";
end h;

entity ysc is
  port (nflosnqjkx : inout time_vector(3 to 0));
end ysc;

library ieee;
use ieee.std_logic_1164.all;

architecture umkl of ysc is
  signal awdwor : string(4 to 5);
  signal o : bit_vector(0 to 3);
  signal fh : character;
  signal bxmbdeggh : real_vector(2 downto 0);
  signal abuswisr : boolean_vector(0 to 2);
  signal alo : std_logic_vector(1 downto 0);
  signal aomsbfwt : std_logic_vector(2 to 4);
  signal mw : string(4 to 5);
  signal c : bit_vector(0 to 3);
  signal hmveegmpnp : character;
begin
  vreascr : entity work.gohecqgcqn
    port map (vszytxo => hmveegmpnp, itolvk => c, l => mw);
  k : entity work.dn
    port map (syto => aomsbfwt, toejtdr => alo, ktryurphbu => abuswisr, nfoycxmv => bxmbdeggh);
  rlhap : entity work.gohecqgcqn
    port map (vszytxo => fh, itolvk => o, l => awdwor);
  
  -- Single-driven assignments
  nflosnqjkx <= (others => 0 ns);
  abuswisr <= (TRUE, TRUE, TRUE);
  
  -- Multi-driven assignments
  aomsbfwt <= "-UL";
  aomsbfwt <= "XU-";
  aomsbfwt <= "HZ-";
  alo <= "ZU";
end umkl;



-- Seed after: 9035251756916389472,17047277710231705797

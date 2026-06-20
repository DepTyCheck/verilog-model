-- Seed: 1774855238096614305,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity tchppe is
  port (yhuelt : inout std_logic; v : linkage std_logic_vector(2 to 4));
end tchppe;

architecture twnjbuuud of tchppe is
  
begin
  
end twnjbuuud;

entity iqsjgl is
  port (zmickwm : buffer bit; ujjyqmdxmk : buffer boolean_vector(3 to 2));
end iqsjgl;

library ieee;
use ieee.std_logic_1164.all;

architecture esbqxl of iqsjgl is
  signal ifymuhb : std_logic_vector(2 to 4);
  signal bu : std_logic;
  signal pjybn : std_logic_vector(2 to 4);
  signal szktrzloi : std_logic_vector(2 to 4);
  signal c : std_logic;
begin
  kiver : entity work.tchppe
    port map (yhuelt => c, v => szktrzloi);
  wtmspdbcl : entity work.tchppe
    port map (yhuelt => c, v => pjybn);
  oldbg : entity work.tchppe
    port map (yhuelt => bu, v => ifymuhb);
  ac : entity work.tchppe
    port map (yhuelt => c, v => szktrzloi);
  
  -- Single-driven assignments
  ujjyqmdxmk <= (others => TRUE);
end esbqxl;

entity puomtnnfci is
  port (qo : inout integer; nssyxuf : buffer integer_vector(0 to 2); kkjlnkyrxm : inout character);
end puomtnnfci;

library ieee;
use ieee.std_logic_1164.all;

architecture yvy of puomtnnfci is
  signal lf : boolean_vector(3 to 2);
  signal kpccctn : bit;
  signal zbpwxhmng : std_logic_vector(2 to 4);
  signal dmeshzn : std_logic;
begin
  vzlvb : entity work.tchppe
    port map (yhuelt => dmeshzn, v => zbpwxhmng);
  lnaoyvn : entity work.iqsjgl
    port map (zmickwm => kpccctn, ujjyqmdxmk => lf);
  
  -- Single-driven assignments
  kkjlnkyrxm <= 'm';
  qo <= 323;
  nssyxuf <= (8#1_0_0_3#, 2#1#, 16#2#);
  
  -- Multi-driven assignments
  dmeshzn <= 'X';
  zbpwxhmng <= ('L', 'H', '1');
  zbpwxhmng <= "UWX";
end yvy;

entity djtc is
  port (leiijr : out bit; lhidkqktnf : buffer integer; dmpzmt : out time);
end djtc;

architecture aowxqc of djtc is
  signal igpsfsdfh : character;
  signal ikr : integer_vector(0 to 2);
begin
  t : entity work.puomtnnfci
    port map (qo => lhidkqktnf, nssyxuf => ikr, kkjlnkyrxm => igpsfsdfh);
  
  -- Single-driven assignments
  dmpzmt <= 1 min;
end aowxqc;



-- Seed after: 6793899459268828881,3924983747739634027

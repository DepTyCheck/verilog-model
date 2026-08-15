-- Seed: 16002776005057498262,2230106469645304029

entity uukix is
  port (boufwpzsy : in time; qokdlhaznm : buffer time; ooqdq : out real_vector(1 downto 0));
end uukix;

architecture regzyuax of uukix is
  
begin
  -- Single-driven assignments
  ooqdq <= (3_4_0.12114, 3_1_3_3_2.40124);
  qokdlhaznm <= 2_1_1 ps;
end regzyuax;

library ieee;
use ieee.std_logic_1164.all;

entity gvqqhmend is
  port (aj : buffer integer_vector(1 downto 4); hccmrvxkkp : linkage std_logic; ki : linkage time_vector(3 downto 4));
end gvqqhmend;

architecture qbwilsll of gvqqhmend is
  signal fnlfark : real_vector(1 downto 0);
  signal as : time;
  signal uxhr : real_vector(1 downto 0);
  signal fp : time;
  signal perrhgo : time;
begin
  fpijobzgpt : entity work.uukix
    port map (boufwpzsy => perrhgo, qokdlhaznm => fp, ooqdq => uxhr);
  ndnu : entity work.uukix
    port map (boufwpzsy => perrhgo, qokdlhaznm => as, ooqdq => fnlfark);
  
  -- Single-driven assignments
  aj <= (others => 0);
  perrhgo <= 1_4 ns;
end qbwilsll;

library ieee;
use ieee.std_logic_1164.all;

entity qosohkrs is
  port (rmauntcbq : out std_logic_vector(3 downto 3); papvrszfcq : out integer);
end qosohkrs;

library ieee;
use ieee.std_logic_1164.all;

architecture ifwj of qosohkrs is
  signal s : real_vector(1 downto 0);
  signal foht : time;
  signal yo : time_vector(3 downto 4);
  signal nziohjrdjq : std_logic;
  signal ka : integer_vector(1 downto 4);
  signal u : real_vector(1 downto 0);
  signal qd : time;
  signal ktqosubktz : time;
begin
  xcst : entity work.uukix
    port map (boufwpzsy => ktqosubktz, qokdlhaznm => qd, ooqdq => u);
  hqzky : entity work.gvqqhmend
    port map (aj => ka, hccmrvxkkp => nziohjrdjq, ki => yo);
  mxmcgqxk : entity work.uukix
    port map (boufwpzsy => foht, qokdlhaznm => ktqosubktz, ooqdq => s);
  
  -- Single-driven assignments
  foht <= foht;
  papvrszfcq <= 2_1;
  
  -- Multi-driven assignments
  rmauntcbq <= rmauntcbq;
  rmauntcbq <= rmauntcbq;
  nziohjrdjq <= 'H';
  rmauntcbq <= rmauntcbq;
end ifwj;



-- Seed after: 10171995763811523087,2230106469645304029

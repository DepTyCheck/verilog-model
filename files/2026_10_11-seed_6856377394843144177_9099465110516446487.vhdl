-- Seed: 6856377394843144177,9099465110516446487

library ieee;
use ieee.std_logic_1164.all;

entity hwqaerfctm is
  port (y : in std_logic; chplbgtros : out std_logic_vector(2 to 3));
end hwqaerfctm;

architecture fbkorx of hwqaerfctm is
  
begin
  
end fbkorx;

library ieee;
use ieee.std_logic_1164.all;

entity ccnridoxsh is
  port (jnqibomre : out time; qd : buffer std_logic_vector(3 downto 2));
end ccnridoxsh;

library ieee;
use ieee.std_logic_1164.all;

architecture i of ccnridoxsh is
  signal kzajekg : std_logic;
  signal ebagzclhfz : std_logic_vector(2 to 3);
  signal eaovto : std_logic_vector(2 to 3);
  signal xukmbrtj : std_logic;
begin
  gshpbnbeko : entity work.hwqaerfctm
    port map (y => xukmbrtj, chplbgtros => eaovto);
  zeoj : entity work.hwqaerfctm
    port map (y => xukmbrtj, chplbgtros => ebagzclhfz);
  f : entity work.hwqaerfctm
    port map (y => kzajekg, chplbgtros => eaovto);
  
  -- Single-driven assignments
  jnqibomre <= 2020 ms;
  
  -- Multi-driven assignments
  eaovto <= "WZ";
  qd <= qd;
  xukmbrtj <= kzajekg;
end i;

entity fytxnvtk is
  port (bmwzccy : in integer);
end fytxnvtk;

library ieee;
use ieee.std_logic_1164.all;

architecture zyflcdmvx of fytxnvtk is
  signal z : std_logic;
  signal kkqwztxqrz : std_logic_vector(2 to 3);
  signal gplpww : time;
begin
  qnn : entity work.ccnridoxsh
    port map (jnqibomre => gplpww, qd => kkqwztxqrz);
  qpw : entity work.hwqaerfctm
    port map (y => z, chplbgtros => kkqwztxqrz);
  xv : entity work.hwqaerfctm
    port map (y => z, chplbgtros => kkqwztxqrz);
end zyflcdmvx;

entity pdef is
  port (kdqcunsnbx : in character);
end pdef;

architecture stqyuoa of pdef is
  signal kugwqgjkoq : integer;
  signal jcwegxdbb : integer;
begin
  afsvoxupsd : entity work.fytxnvtk
    port map (bmwzccy => jcwegxdbb);
  vhivd : entity work.fytxnvtk
    port map (bmwzccy => jcwegxdbb);
  rcaozmtz : entity work.fytxnvtk
    port map (bmwzccy => kugwqgjkoq);
  sndeoidfmj : entity work.fytxnvtk
    port map (bmwzccy => kugwqgjkoq);
  
  -- Single-driven assignments
  jcwegxdbb <= 8#1061#;
end stqyuoa;



-- Seed after: 14860888043373293690,9099465110516446487

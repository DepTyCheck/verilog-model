-- Seed: 10611928284019933559,6697892553037813751

entity ekbqpfyq is
  port (obdbzprkq : inout time; ohfispqeq : out integer_vector(1 downto 1));
end ekbqpfyq;

architecture migzvdp of ekbqpfyq is
  
begin
  -- Single-driven assignments
  ohfispqeq <= (others => 24);
end migzvdp;

library ieee;
use ieee.std_logic_1164.all;

entity occa is
  port (fix : inout boolean_vector(3 to 0); suvywo : in integer; zui : linkage std_logic);
end occa;

architecture v of occa is
  signal nsh : integer_vector(1 downto 1);
  signal hcbtq : time;
  signal eljdv : integer_vector(1 downto 1);
  signal skkeaa : time;
  signal gbami : integer_vector(1 downto 1);
  signal jpidhz : time;
begin
  mulijlo : entity work.ekbqpfyq
    port map (obdbzprkq => jpidhz, ohfispqeq => gbami);
  ipcxdje : entity work.ekbqpfyq
    port map (obdbzprkq => skkeaa, ohfispqeq => eljdv);
  jsknp : entity work.ekbqpfyq
    port map (obdbzprkq => hcbtq, ohfispqeq => nsh);
  
  -- Single-driven assignments
  fix <= (others => TRUE);
end v;



-- Seed after: 16303444669291122302,6697892553037813751

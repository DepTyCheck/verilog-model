-- Seed: 10251981034896686536,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (vm : out std_logic; sex : inout time; olpo : in std_logic; gc : out integer_vector(0 downto 0));
end g;

architecture gr of g is
  
begin
  -- Single-driven assignments
  gc <= (others => 3_0);
  sex <= sex;
  
  -- Multi-driven assignments
  vm <= olpo;
  vm <= olpo;
  vm <= olpo;
end gr;

entity vmgz is
  port (tj : linkage time; mczd : out real_vector(1 downto 1); klv : buffer time; pgdz : in bit_vector(1 downto 4));
end vmgz;

library ieee;
use ieee.std_logic_1164.all;

architecture wajlpiys of vmgz is
  signal reohfyyxx : integer_vector(0 downto 0);
  signal flrhe : std_logic;
  signal muc : integer_vector(0 downto 0);
  signal htcfvjii : std_logic;
  signal rzgdc : time;
  signal haelilxlcq : std_logic;
begin
  opexae : entity work.g
    port map (vm => haelilxlcq, sex => rzgdc, olpo => htcfvjii, gc => muc);
  wnolw : entity work.g
    port map (vm => flrhe, sex => klv, olpo => haelilxlcq, gc => reohfyyxx);
  
  -- Multi-driven assignments
  haelilxlcq <= '-';
  haelilxlcq <= 'H';
end wajlpiys;

library ieee;
use ieee.std_logic_1164.all;

entity npmnzoifkm is
  port (jxrnaxyyk : in std_logic; sita : linkage boolean; dovs : buffer time; ehwau : in boolean_vector(4 downto 3));
end npmnzoifkm;

architecture qudujms of npmnzoifkm is
  
begin
  
end qudujms;

entity dyhftnr is
  port (fehf : in integer);
end dyhftnr;

library ieee;
use ieee.std_logic_1164.all;

architecture yflmjfse of dyhftnr is
  signal ijm : boolean_vector(4 downto 3);
  signal ibkz : time;
  signal wivi : boolean;
  signal ck : boolean_vector(4 downto 3);
  signal hyrff : time;
  signal j : boolean;
  signal vto : std_logic;
begin
  nl : entity work.npmnzoifkm
    port map (jxrnaxyyk => vto, sita => j, dovs => hyrff, ehwau => ck);
  ztnlpk : entity work.npmnzoifkm
    port map (jxrnaxyyk => vto, sita => wivi, dovs => ibkz, ehwau => ijm);
  
  -- Single-driven assignments
  ck <= (TRUE, FALSE);
  
  -- Multi-driven assignments
  vto <= 'H';
  vto <= vto;
  vto <= vto;
end yflmjfse;



-- Seed after: 9855403367771705336,7311216359267151659

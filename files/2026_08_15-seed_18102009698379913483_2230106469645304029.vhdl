-- Seed: 18102009698379913483,2230106469645304029

entity cnanrm is
  port (vkgmgfdt : buffer integer_vector(1 to 4); mcrixu : in time);
end cnanrm;

architecture e of cnanrm is
  
begin
  
end e;

library ieee;
use ieee.std_logic_1164.all;

entity odwdnhz is
  port (oggemlaauh : in std_logic_vector(4 to 4); cmbubr : in time; fjspikdpqt : buffer std_logic_vector(4 to 4));
end odwdnhz;

architecture ccuxcw of odwdnhz is
  
begin
  -- Multi-driven assignments
  fjspikdpqt <= fjspikdpqt;
  fjspikdpqt <= fjspikdpqt;
  fjspikdpqt <= (others => 'H');
  fjspikdpqt <= fjspikdpqt;
end ccuxcw;

entity mm is
  port (ekvppe : buffer integer; lhdxjhtobp : inout boolean_vector(2 downto 4); eu : out string(5 to 5); z : linkage string(5 downto 4));
end mm;

library ieee;
use ieee.std_logic_1164.all;

architecture dn of mm is
  signal zsqudh : integer_vector(1 to 4);
  signal hjzfd : time;
  signal ebylfvlkxl : std_logic_vector(4 to 4);
  signal odi : std_logic_vector(4 to 4);
  signal igqgiks : time;
  signal lh : integer_vector(1 to 4);
begin
  txdvets : entity work.cnanrm
    port map (vkgmgfdt => lh, mcrixu => igqgiks);
  uin : entity work.odwdnhz
    port map (oggemlaauh => odi, cmbubr => igqgiks, fjspikdpqt => odi);
  gtir : entity work.odwdnhz
    port map (oggemlaauh => ebylfvlkxl, cmbubr => hjzfd, fjspikdpqt => odi);
  ohyqd : entity work.cnanrm
    port map (vkgmgfdt => zsqudh, mcrixu => hjzfd);
  
  -- Single-driven assignments
  eu <= eu;
  hjzfd <= 2 ms;
  ekvppe <= ekvppe;
  lhdxjhtobp <= lhdxjhtobp;
  
  -- Multi-driven assignments
  odi <= "Z";
  odi <= odi;
  odi <= odi;
  odi <= "1";
end dn;



-- Seed after: 12762702207060155584,2230106469645304029

-- Seed: 16299582975630221692,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (bk : inout time; gtsuppwskp : out boolean_vector(3 downto 3); izldytni : buffer std_logic);
end d;

architecture gfuvqca of d is
  
begin
  -- Single-driven assignments
  bk <= 3 sec;
  gtsuppwskp <= (others => TRUE);
end gfuvqca;

library ieee;
use ieee.std_logic_1164.all;

entity tvyngz is
  port (bwsvold : linkage real; tecoaswev : in std_logic);
end tvyngz;

library ieee;
use ieee.std_logic_1164.all;

architecture cfffvq of tvyngz is
  signal bwpptw : boolean_vector(3 downto 3);
  signal bhbmjkjbe : time;
  signal rbegovhmuz : boolean_vector(3 downto 3);
  signal disu : time;
  signal lpcapkfa : boolean_vector(3 downto 3);
  signal ymcdpmck : time;
  signal he : std_logic;
  signal tohfxlc : boolean_vector(3 downto 3);
  signal nvnc : time;
begin
  rve : entity work.d
    port map (bk => nvnc, gtsuppwskp => tohfxlc, izldytni => he);
  h : entity work.d
    port map (bk => ymcdpmck, gtsuppwskp => lpcapkfa, izldytni => he);
  ltiuvsxmg : entity work.d
    port map (bk => disu, gtsuppwskp => rbegovhmuz, izldytni => he);
  dszwtdhtj : entity work.d
    port map (bk => bhbmjkjbe, gtsuppwskp => bwpptw, izldytni => he);
  
  -- Multi-driven assignments
  he <= '0';
  he <= tecoaswev;
  he <= tecoaswev;
end cfffvq;

entity krhrkqcnho is
  port (olck : inout severity_level; bkqrqg : out time);
end krhrkqcnho;

library ieee;
use ieee.std_logic_1164.all;

architecture b of krhrkqcnho is
  signal pdtercfbdi : std_logic;
  signal q : boolean_vector(3 downto 3);
  signal rfyks : time;
  signal ylbsbskcwb : real;
  signal gsexpdnld : boolean_vector(3 downto 3);
  signal jtjf : std_logic;
  signal mcxhhmme : real;
begin
  p : entity work.tvyngz
    port map (bwsvold => mcxhhmme, tecoaswev => jtjf);
  rothgjmoa : entity work.d
    port map (bk => bkqrqg, gtsuppwskp => gsexpdnld, izldytni => jtjf);
  wzuuvbroz : entity work.tvyngz
    port map (bwsvold => ylbsbskcwb, tecoaswev => jtjf);
  whxzvhtkkp : entity work.d
    port map (bk => rfyks, gtsuppwskp => q, izldytni => pdtercfbdi);
  
  -- Single-driven assignments
  olck <= olck;
end b;



-- Seed after: 3849489385742463242,2230106469645304029

-- Seed: 17497172435460669808,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity gwkitzdrj is
  port (hnhhpbqckf : in bit; pfrkd : buffer std_logic_vector(1 to 4));
end gwkitzdrj;

architecture h of gwkitzdrj is
  
begin
  -- Multi-driven assignments
  pfrkd <= "ZU0H";
end h;

entity kqgzqfafa is
  port (no : in integer);
end kqgzqfafa;

library ieee;
use ieee.std_logic_1164.all;

architecture poreril of kqgzqfafa is
  signal lsmbcvv : std_logic_vector(1 to 4);
  signal wwfhuj : bit;
  signal nl : std_logic_vector(1 to 4);
  signal ugkyc : bit;
begin
  mjlotphshb : entity work.gwkitzdrj
    port map (hnhhpbqckf => ugkyc, pfrkd => nl);
  cxljoagm : entity work.gwkitzdrj
    port map (hnhhpbqckf => wwfhuj, pfrkd => lsmbcvv);
  
  -- Single-driven assignments
  ugkyc <= '0';
  wwfhuj <= '0';
  
  -- Multi-driven assignments
  nl <= ('Z', 'Z', 'X', 'H');
  nl <= "10UW";
end poreril;

library ieee;
use ieee.std_logic_1164.all;

entity xik is
  port (fuavxyahs : inout std_logic_vector(4 downto 1));
end xik;

library ieee;
use ieee.std_logic_1164.all;

architecture dsyp of xik is
  signal toqhhtdmv : std_logic_vector(1 to 4);
  signal bhbr : bit;
  signal hrhcgivjp : std_logic_vector(1 to 4);
  signal bfulfma : bit;
  signal skgrwsbh : std_logic_vector(1 to 4);
  signal bv : bit;
  signal uavrwubhpa : std_logic_vector(1 to 4);
  signal ylz : bit;
begin
  zfug : entity work.gwkitzdrj
    port map (hnhhpbqckf => ylz, pfrkd => uavrwubhpa);
  lttnarmct : entity work.gwkitzdrj
    port map (hnhhpbqckf => bv, pfrkd => skgrwsbh);
  zvtkmitg : entity work.gwkitzdrj
    port map (hnhhpbqckf => bfulfma, pfrkd => hrhcgivjp);
  mdduwvuxm : entity work.gwkitzdrj
    port map (hnhhpbqckf => bhbr, pfrkd => toqhhtdmv);
end dsyp;



-- Seed after: 8140571751537629034,17924494779688682807

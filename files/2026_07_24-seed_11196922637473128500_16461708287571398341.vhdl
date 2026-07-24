-- Seed: 11196922637473128500,16461708287571398341

entity ovcbbe is
  port (k : buffer real; dicgd : buffer bit; iqcsp : linkage time);
end ovcbbe;

architecture b of ovcbbe is
  
begin
  -- Single-driven assignments
  k <= 8#70722.1_1_3#;
end b;

entity la is
  port (lcpovz : linkage bit_vector(1 downto 1); k : in real; t : out boolean_vector(2 downto 2));
end la;

architecture cnkfyn of la is
  signal kpeyh : time;
  signal bhcinabh : bit;
  signal ovto : real;
  signal vdrk : time;
  signal nziktyq : bit;
  signal vpagu : real;
begin
  i : entity work.ovcbbe
    port map (k => vpagu, dicgd => nziktyq, iqcsp => vdrk);
  g : entity work.ovcbbe
    port map (k => ovto, dicgd => bhcinabh, iqcsp => kpeyh);
  
  -- Single-driven assignments
  t <= (others => TRUE);
end cnkfyn;

entity it is
  port (nxf : linkage time_vector(0 to 4));
end it;

architecture kkuonpyw of it is
  signal rjjbef : boolean_vector(2 downto 2);
  signal cs : real;
  signal dvmg : bit_vector(1 downto 1);
  signal covwyjbx : time;
  signal xli : bit;
  signal sfmlpnsnq : real;
  signal uniqy : time;
  signal nkuimupq : bit;
  signal xvugnzz : real;
begin
  qvcmieywa : entity work.ovcbbe
    port map (k => xvugnzz, dicgd => nkuimupq, iqcsp => uniqy);
  w : entity work.ovcbbe
    port map (k => sfmlpnsnq, dicgd => xli, iqcsp => covwyjbx);
  xwstzzxl : entity work.la
    port map (lcpovz => dvmg, k => cs, t => rjjbef);
end kkuonpyw;



-- Seed after: 8811282541309997327,16461708287571398341

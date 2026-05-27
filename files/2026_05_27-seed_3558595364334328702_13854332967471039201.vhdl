-- Seed: 3558595364334328702,13854332967471039201



entity dcobllrmt is
  port (cxxrc : buffer time; swbmf : out boolean_vector(1 downto 1));
end dcobllrmt;



architecture vsd of dcobllrmt is
  
begin
  
end vsd;



entity alwtxm is
  port (amxkrk : inout real; etdac : out boolean; qxkp : inout integer);
end alwtxm;



architecture gjxtmc of alwtxm is
  signal sbclpgx : boolean_vector(1 downto 1);
  signal ft : time;
  signal mefcaqbu : boolean_vector(1 downto 1);
  signal gnafqag : time;
  signal umeiybyuz : boolean_vector(1 downto 1);
  signal aqidzb : time;
begin
  xafprq : entity work.dcobllrmt
    port map (cxxrc => aqidzb, swbmf => umeiybyuz);
  mydrtfu : entity work.dcobllrmt
    port map (cxxrc => gnafqag, swbmf => mefcaqbu);
  o : entity work.dcobllrmt
    port map (cxxrc => ft, swbmf => sbclpgx);
end gjxtmc;



entity ahsgxjbga is
  port (vziistj : inout boolean);
end ahsgxjbga;



architecture qozra of ahsgxjbga is
  
begin
  
end qozra;



entity at is
  port (pabqeyl : in integer; ycaltpzxz : buffer real; vgser : inout real; dqhwdddprc : in boolean);
end at;



architecture ydjxvlax of at is
  signal qnqch : boolean_vector(1 downto 1);
  signal nke : time;
  signal cbx : boolean_vector(1 downto 1);
  signal rneh : time;
  signal hsccfxmvp : boolean;
begin
  jkgrlc : entity work.ahsgxjbga
    port map (vziistj => hsccfxmvp);
  rcubfztle : entity work.dcobllrmt
    port map (cxxrc => rneh, swbmf => cbx);
  dlhhcxh : entity work.dcobllrmt
    port map (cxxrc => nke, swbmf => qnqch);
end ydjxvlax;



-- Seed after: 13726848341040189003,13854332967471039201

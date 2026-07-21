-- Seed: 13601001873178313414,11481034001933599325

entity ums is
  port (vgffxk : buffer integer_vector(3 downto 1); mrkjulasbt : out time; dl : buffer real);
end ums;

architecture eoarxedww of ums is
  
begin
  
end eoarxedww;

entity xmwi is
  port (wpvlwt : buffer bit_vector(4 downto 0); f : buffer time);
end xmwi;

architecture x of xmwi is
  signal czvmeryf : real;
  signal d : integer_vector(3 downto 1);
  signal nclkpa : real;
  signal czpicgae : time;
  signal xvhviegu : integer_vector(3 downto 1);
  signal yja : real;
  signal wplafomd : time;
  signal bxhcdfv : integer_vector(3 downto 1);
begin
  errc : entity work.ums
    port map (vgffxk => bxhcdfv, mrkjulasbt => wplafomd, dl => yja);
  icpu : entity work.ums
    port map (vgffxk => xvhviegu, mrkjulasbt => czpicgae, dl => nclkpa);
  ysvf : entity work.ums
    port map (vgffxk => d, mrkjulasbt => f, dl => czvmeryf);
  
  -- Single-driven assignments
  wpvlwt <= ('1', '1', '1', '1', '1');
end x;



-- Seed after: 10272678070383309739,11481034001933599325

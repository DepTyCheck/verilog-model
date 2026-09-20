-- Seed: 9960540147563731786,18037650846010261179

entity rxbenkhv is
  port (yv : out time; apesoqwkq : buffer time);
end rxbenkhv;

architecture ciujfxkv of rxbenkhv is
  
begin
  -- Single-driven assignments
  apesoqwkq <= yv;
  yv <= yv;
end ciujfxkv;

entity d is
  port (ns : linkage integer_vector(2 to 1); w : buffer real);
end d;

architecture niododca of d is
  signal voxn : time;
  signal aefhy : time;
  signal cbresvlj : time;
  signal im : time;
begin
  qfxywuyfp : entity work.rxbenkhv
    port map (yv => im, apesoqwkq => cbresvlj);
  o : entity work.rxbenkhv
    port map (yv => aefhy, apesoqwkq => voxn);
end niododca;

entity pjl is
  port (jrxjxo : buffer real);
end pjl;

architecture nbrqmrw of pjl is
  signal algpqyzx : time;
  signal icjcptsdsr : time;
  signal zsjes : real;
  signal liwb : integer_vector(2 to 1);
begin
  jprwul : entity work.d
    port map (ns => liwb, w => zsjes);
  xo : entity work.rxbenkhv
    port map (yv => icjcptsdsr, apesoqwkq => algpqyzx);
  
  -- Single-driven assignments
  jrxjxo <= 301.0_4_4_0;
end nbrqmrw;



-- Seed after: 15355739654466099658,18037650846010261179
